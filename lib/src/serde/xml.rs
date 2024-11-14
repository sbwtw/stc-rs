use crate::context::{ModuleContext, ModuleKind};
use crate::parser::{ParserBuilder, StLexerBuilder};

use serde::{Deserialize, Serialize};
use std::str::FromStr;
use uuid::Uuid;

#[derive(Serialize, Deserialize, Debug)]
pub enum ProjectType {
    App,
    Library,
}

impl Default for ProjectType {
    fn default() -> Self {
        Self::App
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Project {
    #[serde(default, rename = "pou-list")]
    pub pou_list: POUList,
    #[serde(rename = "@name")]
    pub name: String,
    #[serde(rename = "@type", default)]
    pub project_type: ProjectType,
    #[serde(rename = "@namespace")]
    pub namespace: Option<String>,
}

impl From<Project> for ModuleContext {
    fn from(proj: Project) -> Self {
        let ctx = match proj.project_type {
            ProjectType::App => ModuleContext::new(ModuleKind::Application),
            ProjectType::Library => ModuleContext::new(ModuleKind::Library),
        };
        let mut ctx_write = ctx.write();

        for pou in proj.pou_list.pou {
            let mut lexer = StLexerBuilder::new().build_str(&pou.interface.content);
            let decl = ParserBuilder::default()
                .build()
                .parse_decl(&mut lexer)
                .unwrap();
            let func = ctx_write.add_declaration(
                decl,
                pou.uuid_text
                    .and_then(|s| Uuid::from_str(&s).ok())
                    .unwrap_or(Uuid::nil()),
            );

            if let Some(body) = pou.body {
                let mut lexer = StLexerBuilder::new().build_str(&body.content);
                let body = ParserBuilder::default()
                    .build()
                    .parse_stmt(&mut lexer)
                    .unwrap();
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
#[allow(clippy::upper_case_acronyms)] // Allow upper case POU naming
pub struct POU {
    #[serde(rename = "@uuid-text")]
    pub uuid_text: Option<String>,
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
