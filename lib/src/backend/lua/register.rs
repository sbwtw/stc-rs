use std::collections::HashSet;
use log::warn;
use smallmap::{Map as SmallMap, smallmap};

use crate::parser::StString;

const MAX_REGISTER_ID: u8 = 255;

#[derive(PartialEq, Eq, Clone, Debug, Copy, Hash)]
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
    local_variable_register: SmallMap<StString, Register>,
    local_variable_register_reverse: SmallMap<Register, StString>,
}

impl RegisterManager {
    #[inline]
    pub fn new() -> Self {
        Self {
            virtual_register_cursor: 0,
            real_register_cursor: 0,
            used_real_registers: HashSet::with_capacity(MAX_REGISTER_ID as usize),
            local_variable_register: smallmap![],
            local_variable_register_reverse: smallmap![],
        }
    }

    #[inline]
    pub fn alloc_local_variable(&mut self, v: &StString) -> Register {
        match self.local_variable_register.get(v) {
            Some(r) => *r,
            None => {
                // TODO: allocate virtual
                let r = self.alloc_hard();
                self.local_variable_register.insert(v.clone(), r);
                self.local_variable_register_reverse.insert(r, v.clone());

                r
            }
        }
    }

    /// Reset RegMan, and return register usage is balance
    #[inline]
    pub fn check_and_reset(&mut self) -> bool {
        // free all local variable registers
        for (r, _) in self.local_variable_register_reverse.iter() {
            if let Register::LuaRegister(x) = r {
                self.used_real_registers.remove(x);
            }
        }

        let balanced = self.used_real_registers.is_empty();
        if !balanced {
            warn!("Registers in use: {:?}", self.used_real_registers);
        }

        self.virtual_register_cursor = 0;
        self.real_register_cursor = 0;
        self.used_real_registers.clear();
        self.local_variable_register.clean();
        self.local_variable_register_reverse.clean();

        balanced
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
        // prevent free for local variable register
        if self.local_variable_register_reverse.contains_key(&reg) {
            return;
        }

        match *reg {
            Register::LuaRegister(x) => {
                self.used_real_registers.remove(&x);
            }
            Register::VirtualRegister(_) => {}
        }
    }
}
