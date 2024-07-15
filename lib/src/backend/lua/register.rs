use crate::backend::lua::ConstIdx;
use crate::parser::StString;
use log::warn;
use smallmap::Map as SmallMap;
use std::collections::HashSet;

const MAX_REGISTER_ID: u8 = 255;

/// Register or ConstantIndex
#[derive(PartialEq, Eq, Clone, Debug, Copy, Hash)]
pub enum RK {
    R(Reg),
    K(ConstIdx),
}

/// Register, VirtualRegister or RealRegister
#[derive(PartialEq, Eq, Clone, Debug, Copy, Hash)]
pub enum Reg {
    VR(usize),
    R(u8),
}

impl Reg {
    pub fn num(&self) -> u8 {
        match *self {
            Self::R(x) => x,
            Self::VR(..) => panic!("Can't get number for virtual register"),
        }
    }

    pub fn is_virtual(&self) -> bool {
        matches!(*self, Self::VR(..))
    }

    // for unit test
    #[cfg(test)]
    pub fn from_raw(n: u8) -> Self {
        Self::R(n)
    }
}

pub struct RegisterManager {
    // Cursor point to next free register id
    virtual_register_cursor: usize,
    real_register_cursor: u8,
    used_real_registers: HashSet<u8>,
    local_variable_register: SmallMap<StString, Reg>,
    local_variable_register_reverse: SmallMap<Reg, StString>,
}

impl RegisterManager {
    #[inline]
    pub fn new() -> Self {
        Self {
            virtual_register_cursor: 0,
            real_register_cursor: 0,
            used_real_registers: HashSet::with_capacity(MAX_REGISTER_ID as usize),
            local_variable_register: SmallMap::with_capacity(201),
            local_variable_register_reverse: SmallMap::with_capacity(201),
        }
    }

    #[inline]
    pub fn alloc_local_variable(&mut self, v: &StString) -> Reg {
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
            if let Reg::R(x) = r {
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

    pub fn alloc(&mut self) -> Reg {
        let next = self.virtual_register_cursor;
        self.virtual_register_cursor += 1;

        Reg::VR(next)
    }

    pub fn alloc_hard(&mut self) -> Reg {
        // ensure has free register to allocate
        assert!(self.used_real_registers.len() <= MAX_REGISTER_ID as usize);

        loop {
            if self.used_real_registers.insert(self.real_register_cursor) {
                return Reg::R(self.real_register_cursor);
            }

            self.real_register_cursor += 1;
            self.real_register_cursor %= MAX_REGISTER_ID;
        }
    }

    pub fn alloc_hard_batch(&mut self, count: usize) -> Vec<Reg> {
        let cursor = self.real_register_cursor as usize;
        if cursor + count >= MAX_REGISTER_ID as usize {
            panic!("no more registers!")
        }

        self.real_register_cursor = (cursor + count) as u8;

        let mut r = Vec::with_capacity(count);
        for x in cursor..=cursor + count {
            r.push(Reg::R(x as u8));
            self.used_real_registers.insert(x as u8);
        }

        r
    }

    #[inline]
    pub fn free(&mut self, reg: &Reg) {
        // prevent free for local variable register
        if self.local_variable_register_reverse.contains_key(reg) {
            return;
        }

        match *reg {
            Reg::R(x) => {
                self.used_real_registers.remove(&x);
            }
            Reg::VR(_) => {}
        }
    }
}
