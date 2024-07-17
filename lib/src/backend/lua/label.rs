use crate::ast::SmallVec8;
use crate::parser::StString;
use smallvec::smallvec;
use std::cell::RefCell;
use std::rc::Rc;

pub type LabelPtr = Rc<RefCell<InstLabel>>;

/// Unresolved offset placeholder
#[derive(Default, Debug, Clone)]
pub struct InstLabel {
    /// The instruction position of label
    pub inst_index: Option<usize>,
    /// The name of this label
    pub name: StString,
    /// Record all patch needed instructions
    pub fixup_instructions: SmallVec8<usize>,
}

impl InstLabel {
    pub fn new(name: StString) -> LabelPtr {
        Rc::new(RefCell::new(Self {
            inst_index: None,
            name,
            fixup_instructions: smallvec![],
        }))
    }
}
