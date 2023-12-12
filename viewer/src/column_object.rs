use glib::Object;
use gtk::subclass::prelude::*;
use stc::context::{Function, Prototype};
use stc::prelude::ModuleContext;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};

glib::wrapper! {
    pub struct ColumnObject(ObjectSubclass<ColumnObjectPrivate>);
}

impl ColumnObject {
    pub fn from_ctx(ctx: &ModuleContext) -> Self {
        let s = Self::default();
        let imp = s.imp();

        imp.data.replace(ColumnObjectData::Ctx(ctx.clone()));

        s
    }

    pub fn from_proto(proto: &Prototype) -> Self {
        let s = Self::default();
        let imp = s.imp();

        imp.data.replace(ColumnObjectData::Prototype(proto.clone()));

        s
    }

    pub fn from_func(func: &Function) -> Self {
        let s = Self::default();
        let imp = s.imp();

        imp.data.replace(ColumnObjectData::Function(func.clone()));

        s
    }
}

impl Default for ColumnObject {
    fn default() -> Self {
        Object::new::<Self>()
    }
}

pub enum ColumnObjectData {
    None,
    Ctx(ModuleContext),
    Prototype(Prototype),
    Function(Function),
}

impl Display for ColumnObjectData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => f.write_str("None"),
            Self::Ctx(ctx) => {
                let ctx = ctx.read();
                writeln!(f, "{}", ctx)?;
                writeln!(f, "ID: {}", ctx.id())
            }
            Self::Prototype(proto) => {
                let proto = proto.read().unwrap();

                // name : return_value
                write!(f, "{}", proto.name())?;
                if let Some(ty) = proto.return_value().and_then(|x| x.ty()) {
                    write!(f, " : {}", ty)?;
                }
                writeln!(f)?;

                for v in proto.variables() {
                    writeln!(f, "{}: {}: {:?}", v.flags(), v.name(), v.ty())?;
                }

                Ok(())
            }
            Self::Function(func) => {
                let func = func.read();

                if let Some(code) = func.compiled_code() {
                    writeln!(f, "{}", code)
                } else {
                    writeln!(f, "{}", func.parse_tree())
                }
            }
        }
    }
}

pub struct ColumnObjectPrivate {
    pub data: RefCell<ColumnObjectData>,
}

impl ColumnObjectPrivate {
    pub fn content(&self) -> String {
        self.data.borrow().to_string()
    }
}

impl Default for ColumnObjectPrivate {
    fn default() -> Self {
        Self {
            data: RefCell::new(ColumnObjectData::None),
        }
    }
}

#[glib::object_subclass]
impl ObjectSubclass for ColumnObjectPrivate {
    const NAME: &'static str = "ColumnObject";
    type Type = ColumnObject;
}

impl ObjectImpl for ColumnObjectPrivate {}
