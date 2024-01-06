use serde::{Deserialize, Serialize};
use stc::parser::{StDeclarationParser, StFunctionParser, StLexerBuilder};
use stc::prelude::*;

#[derive(Serialize, Deserialize, Debug)]
pub enum AppType {
    App,
    Library,
}

impl Default for AppType {
    fn default() -> Self {
        Self::App
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Application {
    #[serde(default, rename = "pou-list")]
    pub pou_list: POUList,
    #[serde(rename = "@name")]
    pub name: String,
    #[serde(rename = "@type", default)]
    pub app_type: AppType,
    #[serde(rename = "@namespace")]
    pub namespace: Option<String>,
}

impl From<Application> for ModuleContext {
    fn from(app: Application) -> Self {
        let ctx = match app.app_type {
            AppType::App => ModuleContext::new(ModuleContextScope::Application),
            AppType::Library => ModuleContext::new(ModuleContextScope::Library),
        };
        let mut ctx_write = ctx.write();

        for pou in app.pou_list.pou {
            let lexer = StLexerBuilder::new().build_str(&pou.interface.content);
            let decl = StDeclarationParser::new().parse(lexer).unwrap();
            let func = ctx_write.add_declaration(decl);

            if let Some(body) = pou.body {
                let lexer = StLexerBuilder::new().build_str(&body.content);
                let body = StFunctionParser::new().parse(lexer).unwrap();
                ctx_write.add_function(func, body);
            }
        }

        drop(ctx_write);
        ctx
    }
}

#[derive(Serialize, Deserialize, Debug, Default)]
pub struct POUList {
    pub pou: Vec<POU>,
}

#[derive(Serialize, Deserialize, Debug)]
#[allow(clippy::upper_case_acronyms)]
pub struct POU {
    #[serde(rename = "@guid-text")]
    pub guid_text: Option<String>,
    pub interface: POUInterface,
    pub body: Option<POUBody>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct POUInterface {
    #[serde(rename = "$text")]
    pub content: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct POUBody {
    #[serde(rename = "$text")]
    pub content: String,
}
