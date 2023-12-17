use std::collections::HashSet;

pub type RegisterId = usize;

const MAX_REGISTER_ID: usize = 255;

pub struct RegisterManager {
    register_alloc_cursor: usize,
    used_registers: HashSet<usize>,
}

impl RegisterManager {
    #[inline]
    pub fn new() -> Self {
        Self {
            register_alloc_cursor: 0,
            used_registers: HashSet::with_capacity(MAX_REGISTER_ID),
        }
    }

    pub fn alloc(&mut self) -> RegisterId {
        // ensure has free register to allocate
        assert!(self.used_registers.len() <= MAX_REGISTER_ID);

        loop {
            if self.used_registers.insert(self.register_alloc_cursor) {
                return self.register_alloc_cursor;
            }

            self.register_alloc_cursor += 1;
            self.register_alloc_cursor %= MAX_REGISTER_ID;
        }
    }

    #[inline]
    pub fn free(&mut self, id: &RegisterId) {
        _ = self.used_registers.remove(id)
    }

    #[inline]
    pub fn used_count(&self) -> usize {
        self.used_registers.len()
    }
}
