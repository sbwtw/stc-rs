use crate::prelude::*;

use crate::backend::lua::bytecode::LuaCode;
use crate::backend::CompiledCode;
use std::io;
use std::io::Write;
use std::mem::size_of;

/// Lua signature
const LUA_SIGNATURE: &str = "\x1bLua";

/// data to catch conversion errors
const LUAC_DATA: &[u8] = &[0x19, 0x93, 0x0d, 0x0a, 0x1a, 0x0a];
const LUAC_INT: u64 = 0x5678;
const LUAC_NUMBER: f64 = 370.5;

pub fn lua_dump_function<W: Write>(f: &Function, w: &mut W) -> io::Result<()> {
    let fun_impl = f.read();
    let cc = fun_impl.compiled_code().as_ref().unwrap();

    cc.dump(w)
}

#[inline]
fn lua_dump_int(w: &mut dyn Write, i: i32) -> io::Result<()> {
    lua_dump_size(w, i as u32 as u64)
}

#[inline]
fn lua_dump_size(w: &mut dyn Write, mut data: u64) -> io::Result<()> {
    if data == 0 {
        return lua_dump_byte(w, 0x80);
    }

    let mut buf = [0u8; size_of::<u64>()];
    let mut reverse_index = buf.len() - 1;
    while data != 0 {
        buf[reverse_index] = (data & 0x7f) as u8;
        reverse_index -= 1;

        data >>= 7;
    }

    // mark last byte
    buf[buf.len() - 1] |= 0x80;

    // write all trunked bytes
    lua_dump_bytes(w, &buf[reverse_index + 1..])
}

#[inline]
fn lua_dump_header() -> io::Result<()> {
    Ok(())
}

#[inline]
fn lua_dump_byte(w: &mut dyn Write, b: u8) -> io::Result<()> {
    w.write(&[b]).map(|_| ())
}

#[inline]
fn lua_dump_bytes(w: &mut dyn Write, bytes: &[u8]) -> io::Result<()> {
    w.write_all(bytes).map(|_| ())
}

#[inline]
fn lua_dump_block() -> io::Result<()> {
    Ok(())
}

impl CompiledCode for LuaCode {
    fn dump(&self, w: &mut dyn Write) -> io::Result<()> {
        // Lua header
        lua_dump_bytes(w, LUA_SIGNATURE.as_bytes())?;
        // Lua version, 5.4
        lua_dump_byte(w, 5 * 16 + 4)?;
        // format, now is zero
        lua_dump_byte(w, 0)?;
        // data
        lua_dump_bytes(w, LUAC_DATA)?;
        // size of Lua instruction
        lua_dump_byte(w, 4)?;
        // size of Lua integer
        lua_dump_byte(w, 8)?;
        // size of Lua Number
        lua_dump_byte(w, 8)?;
        // LUAC_INT
        lua_dump_bytes(w, &LUAC_INT.to_le_bytes())?;
        // LUAC_NUMBER
        lua_dump_bytes(w, &LUAC_NUMBER.to_le_bytes())?;

        // TODO: size of UpValues in byte, write 0 in temp
        lua_dump_byte(w, 0)?;
        // TODO: source file name
        lua_dump_int(w, 0)?;
        // TODO: linedefined
        lua_dump_int(w, 0)?;
        // TODO: lastlinedefined
        lua_dump_int(w, 0)?;
        // TODO: numparams
        lua_dump_byte(w, 0)?;
        // TODO: is_vararg
        lua_dump_byte(w, 0)?;
        // TODO: maxstacksize
        lua_dump_byte(w, 0)?;

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::backend::lua::dump::lua_dump_size;

    #[test]
    fn test_lua_dump_size() {
        let mut buf = vec![0u8; 0];
        lua_dump_size(&mut buf, 0).unwrap();
        assert_eq!(0x80, buf[0]);

        let mut buf = vec![0u8; 0];
        lua_dump_size(&mut buf, 1).unwrap();
        assert_eq!(0x81, buf[0]);

        let mut buf = vec![0u8; 0];
        lua_dump_size(&mut buf, 127).unwrap();
        assert_eq!(0xff, buf[0]);

        let mut buf = vec![0u8; 0];
        lua_dump_size(&mut buf, 128).unwrap();
        assert_eq!(0x01, buf[0]);
        assert_eq!(0x80, buf[1]);
    }
}
