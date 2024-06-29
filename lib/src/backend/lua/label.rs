use smallvec::smallvec;
use crate::ast::SmallVec8;
use crate::parser::StString;

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
    pub fn new(name: StString) -> Self {
        Self {
            inst_index: None,
            name,
            fixup_instructions: smallvec![],
        }
    }
}

