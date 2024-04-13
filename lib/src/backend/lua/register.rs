use std::collections::HashSet;

const MAX_REGISTER_ID: u8 = 255;

#[derive(PartialEq, Eq, Clone, Debug, Copy)]
pub enum Register {
    VirtualRegister(usize),
    LuaRegister(u8),
}

impl Register {
    pub fn num(&self) -> u8 {
        match *self {
            Self::LuaRegister(x) => x,
            Self::VirtualRegister(..) => panic!("Can't get number for virtual register"),
        }
    }

    pub fn is_virtual(&self) -> bool {
        matches!(*self, Self::VirtualRegister(..))
    }

    // for unit test
    #[cfg(test)]
    pub fn from_raw(n: u8) -> Self {
        Self::LuaRegister(n)
    }
}

pub struct RegisterManager {
    // Cursor point to next free register id
    virtual_register_cursor: usize,
    real_register_cursor: u8,
    used_real_registers: HashSet<u8>,
}

impl RegisterManager {
    #[inline]
    pub fn new() -> Self {
        Self {
            virtual_register_cursor: 0,
            real_register_cursor: 0,
            used_real_registers: HashSet::with_capacity(MAX_REGISTER_ID as usize),
        }
    }

    pub fn alloc(&mut self) -> Register {
        let next = self.virtual_register_cursor;
        self.virtual_register_cursor += 1;

        Register::VirtualRegister(next)
    }

    pub fn alloc_hard(&mut self) -> Register {
        // ensure has free register to allocate
        assert!(self.used_real_registers.len() <= MAX_REGISTER_ID as usize);

        loop {
            if self.used_real_registers.insert(self.real_register_cursor) {
                return Register::LuaRegister(self.real_register_cursor);
            }

            self.real_register_cursor += 1;
            self.real_register_cursor %= MAX_REGISTER_ID;
        }
    }

    #[inline]
    pub fn free(&mut self, reg: &Register) {
        match *reg {
            Register::LuaRegister(x) => {
                self.used_real_registers.remove(&x);
            }
            Register::VirtualRegister(_) => {}
        }
    }
}
