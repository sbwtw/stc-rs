use crate::ast::{
    AssignExpression, AstVisitorMut, IfStatement, OperatorExpression, Variable, VariableExpression,
};
use crate::codegen::{AccessModeFlags, CodeGenBackend, CodeGenError, TargetCode};
use crate::context::{Function, ModuleContext, Scope, UnitsManager};

use crate::parser::Operator;
use log::*;
use smallvec::{smallvec, SmallVec};
use std::fmt::{Display, Formatter};
use std::rc::Rc;

type RegisterId = usize;

trait LuaInstruction {}

pub struct LuaCode {}

impl Display for LuaCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        unimplemented!()
    }
}

impl TargetCode for LuaCode {}

#[allow(non_camel_case_types)]
enum LuaOpCode {
    // A B, R[A] := R[B]
    OP_MOVE = 0,
    // A sBx, R[A] := sBx
    OP_LOADI = 1,
    // A sBx, R[A] := (lua_Number)xBx
    OP_LOADF = 2,
    // A Bx, R[A] := K[Bx]
    OP_LOADK = 3,
    // A, R[A] := K[extra arg]
    OP_LOADKX = 4,
    OP_LOADFALSE = 5,
    OP_LFALSESKIP = 6,
    OP_LOADTRUE = 7,
    OP_LOADNIL = 8,
    OP_GETUPVAL = 9,
    OP_SETUPVAL = 10,

    OP_GETTABUP = 11,
    OP_GETTABLE = 12,
    OP_GETI = 13,
    OP_GETFIELD = 14,

    OP_SETTABUP = 15,
    OP_SETTABLE = 16,
    OP_SETI = 17,
    OP_SETFIELD = 18,

    OP_NEWTABLE = 19,

    OP_SELF = 20,

    OP_ADDI = 21,

    OP_ADDK = 22,
    OP_SUBK = 23,
    OP_MULK = 24,
    OP_MODK = 25,
    OP_POWK = 26,
    OP_DIVK = 27,
    OP_IDIVK = 28,

    OP_BANDK = 29,
    OP_BORK = 30,
    OP_BXORK = 31,

    OP_SHRI = 32,
    OP_SHLI = 33,

    OP_ADD = 34,
    OP_SUB = 35,
    OP_MUL = 36,
    OP_MOD = 37,
    OP_POW = 38,
    OP_DIV = 39,
    OP_IDIV = 40,

    OP_BAND = 41,
    OP_BOR = 42,
    OP_BXOR = 43,
    OP_SHL = 44,
    OP_SHR = 45,

    OP_MMBIN = 46,
    OP_MMBINI = 47,
    OP_MMBINK = 48,

    OP_UNM = 49,
    OP_BNOT = 50,
    OP_NOT = 51,
    OP_LEN = 52,

    OP_CONCAT = 53,

    OP_CLOSE = 54,
    OP_TBC = 55,
    OP_JMP = 56,
    OP_EQ = 57,
    OP_LT = 58,
    OP_LE = 59,

    OP_EQK = 60,
    OP_EQI = 61,
    OP_LTI = 62,
    OP_LEI = 63,
    OP_GTI = 64,
    OP_GEI = 65,

    OP_TEST = 66,
    OP_TESTSET = 67,

    OP_CALL = 68,
    OP_TAILCALL = 69,

    OP_RETURN = 70,
    OP_RETURN0 = 71,
    OP_RETURN1 = 72,

    OP_FORLOOP = 73,
    OP_FORPREP = 74,

    OP_TFORPREP = 75,
    OP_TFORCALL = 76,
    OP_TFORLOOP = 77,

    OP_SETLIST = 78,

    OP_CLOSURE = 79,

    OP_VARARG = 80,

    OP_VARARGPREP = 81,

    OP_EXTRAARG = 82,
}

#[derive(Clone)]
pub struct LuaBackendAttribute {
    variable: Option<Rc<Variable>>,
    register: Option<RegisterId>,
    scope: Option<Scope>,
    error: bool,
    access_mode: AccessModeFlags,
}

impl Default for LuaBackendAttribute {
    fn default() -> Self {
        Self {
            variable: None,
            register: None,
            scope: None,
            error: false,
            access_mode: AccessModeFlags::NONE,
        }
    }
}

pub struct LuaBackend {
    mgr: UnitsManager,
    app: ModuleContext,
    instructions: Vec<Box<dyn LuaInstruction>>,
    attributes: SmallVec<[LuaBackendAttribute; 16]>,
    local_function: Option<Function>,
}

impl LuaBackend {
    pub fn new(mgr: UnitsManager, app: ModuleContext) -> Self {
        Self {
            mgr,
            app,
            instructions: vec![],
            attributes: smallvec![],
            local_function: None,
        }
    }

    fn push_attribute_with_scope(&mut self, scope: Scope) {
        let attr = LuaBackendAttribute {
            scope: Some(scope),
            ..Default::default()
        };

        self.attributes.push(attr)
    }

    fn push_access_attribute(&mut self, access: AccessModeFlags) {
        let attr = LuaBackendAttribute {
            scope: self.top_attribute().scope.clone(),
            access_mode: access,
            ..Default::default()
        };

        self.attributes.push(attr)
    }

    fn push_default_attribute(&mut self) {
        let attr = LuaBackendAttribute {
            scope: self.top_attribute().scope.clone(),
            ..Default::default()
        };

        self.attributes.push(attr);
    }

    fn pop_attribute(&mut self) -> LuaBackendAttribute {
        self.attributes.pop().unwrap()
    }

    fn top_attribute(&mut self) -> &mut LuaBackendAttribute {
        self.attributes.last_mut().unwrap()
    }
}

impl CodeGenBackend for LuaBackend {
    type Label = usize;

    fn take_code(&mut self) -> Box<dyn TargetCode> {
        Box::new(LuaCode {})
    }

    fn define_label<S: AsRef<str>>(&mut self, label: Option<S>) -> Self::Label {
        0
    }

    fn gen_function(&mut self, func: usize) -> Result<(), CodeGenError> {
        let f = self
            .app
            .read()
            .get_function(func)
            .ok_or(CodeGenError::FunctionNotDefined(func))?
            .clone();

        self.local_function = Some(f.clone());

        let app_id = self.app.read().id();
        let fun_scope = Scope::new(Some(self.mgr.clone()), Some(app_id), Some(func));

        let mut fun = f.write().unwrap();
        self.push_attribute_with_scope(fun_scope);
        self.visit_statement_mut(fun.body_mut());
        self.pop_attribute();

        Ok(())
    }

    fn gen_variable_load(&mut self, variable: &mut Variable) {
        todo!()
    }

    fn gen_operator(&mut self, operator: &mut OperatorExpression) {
        let operands = operator.operands_mut();

        self.visit_expression_mut(&mut operands[0]);
    }
}

impl AstVisitorMut for LuaBackend {
    fn visit_variable_expression_mut(&mut self, variable: &mut VariableExpression) {
        trace!("LuaGen: variable expression: {}", variable);

        let scope = self.top_attribute().scope.as_ref().unwrap();
        self.top_attribute().variable = scope.find_variable(variable.name());
    }

    fn visit_if_statement_mut(&mut self, ifst: &mut IfStatement) {
        trace!("LuaGen: if statement: {}", ifst.condition());

        let cond_true = self.define_label(Some("if-true"));
        let cond_false = self.define_label(Some("if-false"));

        self.visit_expression_mut(ifst.condition_mut());

        if let Some(then_ctrl) = ifst.then_controlled_mut() {
            self.visit_statement_mut(then_ctrl);
        }
    }

    fn visit_operator_expression_mut(&mut self, operator: &mut OperatorExpression) {
        trace!("LuaGen: operator expression: {}", operator);

        let op = *operator.op();
        let operands = operator.operands_mut();

        match op {
            // binary compare operators
            Operator::Less
            | Operator::LessEqual
            | Operator::Equal
            | Operator::NotEqual
            | Operator::Greater
            | Operator::GreaterEqual => {
                self.push_access_attribute(AccessModeFlags::READ);
                self.visit_expression_mut(&mut operands[0]);
                self.pop_attribute();

                self.push_access_attribute(AccessModeFlags::READ);
                self.visit_expression_mut(&mut operands[1]);
                self.pop_attribute();
            }
            _ => unreachable!(),
        }
    }

    fn visit_assign_expression_mut(&mut self, assign: &mut AssignExpression) {
        trace!("LuaGen: assignment expression: {}", assign);
    }
}
