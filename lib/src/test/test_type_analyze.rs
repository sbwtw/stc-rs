use crate::parser::{ParserBuilder, StLexerBuilder};
use crate::prelude::*;
use crate::serde::Application;

use crate::analysis::TypeAnalyzer;
use quick_xml::de::from_str;

#[test]
fn test_type_analyze() {
    let app: Application = from_str(include_str!("test_projects/test_proj1.xml")).unwrap();
    let mgr = UnitsManager::new();
    let ctx: ModuleContext = app.into();
    let ctx_id = ctx.read().id();
    mgr.write().add_context(ctx.clone());
    mgr.write().set_active_application(Some(ctx.read().id()));

    let stmt_str = "c := a + b;";
    let mut lexer = StLexerBuilder::new().build_str(stmt_str);
    let parser = ParserBuilder::default().build();
    let mut stmt = parser.parse_stmt(&mut lexer).unwrap();

    let mut type_analyzer = TypeAnalyzer::new();
    type_analyzer.analyze_statement(&mut stmt, mgr.module_scope(ctx_id));

    // ensure statement is expression statement
    assert!(matches!(stmt.kind, StmtKind::Expr(..)));

    // ensure expression is assignment
    let expr = match &stmt.kind {
        StmtKind::Expr(expr) => expr.expr(),
        _ => panic!(),
    };
    assert!(matches!(expr.kind, ExprKind::Assign(..)));

    // get assign expr
    let assign_expr = match &expr.kind {
        ExprKind::Assign(assign) => assign,
        _ => panic!(),
    };

    // test left side type
    assert!(assign_expr.left().ty().is_some());
    assert_eq!(
        *assign_expr.left().ty().unwrap().type_class(),
        TypeClass::Real
    );

    // TODO: test right side type, it's an operator expression
}
