use std::io;
use std::io::Write;

use byteorder::{LittleEndian, WriteBytesExt};

use crate::backend::CompiledCode;
use crate::backend::lua::bytecode::LuaCompiledCode;

use super::{LuaConstants, utils::*};
use super::{Function, LuaBackend, Prototype};

/// Lua signature
const LUA_SIGNATURE: &str = "\x1bLua";

/// data to catch conversion errors
const LUAC_DATA: &[u8] = &[0x19, 0x93, 0x0d, 0x0a, 0x1a, 0x0a];
const LUAC_INT: u64 = 0x5678;
// 0x4077280000000000
const LUAC_NUMBER: f64 = 370.5;

pub fn lua_dump_module(backend: &LuaBackend, w: &mut dyn Write) -> io::Result<()> {
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

    // size of UpValues in 1 byte, TODO: hard-coded 1
    lua_dump_byte(w, 1)?;

    // Start to dump functions
    // get main function
    let app = backend.current_application();
    let app_clone = app.clone();
    let app_read = app.read();
    let main_proto = app_read.find_declaration_by_name(&"main".into());
    if let Some(p) = main_proto {
        let main_id = p.read().unwrap().id();
        let main_func = app_clone.read().get_function(main_id).cloned();

        if let Some(f) = main_func {
            lua_dump_function(backend, p, &f, w)?;
        }
    }

    Ok(())
}

fn lua_dump_function(
    b: &LuaBackend,
    p: &Prototype,
    f: &Function,
    w: &mut dyn Write,
) -> io::Result<()> {
    // TODO: source file name
    lua_dump_string(w, None)?;
    // TODO: linedefined
    lua_dump_size(w, 0)?;
    // TODO: lastlinedefined
    lua_dump_size(w, 0)?;
    // numparams
    lua_dump_byte(w, num_params(p))?;
    // is_vararg
    let r = if is_vararg(p) { 1 } else { 0 };
    lua_dump_byte(w, r)?;
    // maxstacksize of proto
    lua_dump_byte(w, max_stack_size(p))?;

    // Dump size of code
    let f = f.read();
    let code = f.compiled_code().as_ref().unwrap();
    let lua_code = code.as_any().downcast_ref::<LuaCompiledCode>().unwrap();
    lua_dump_size(w, lua_code.byte_codes().len() as u64)?;

    // Dump Code
    for c in lua_code.byte_codes() {
        w.write_u32::<LittleEndian>(c.encode())?;
    }

    // Dump size of constants
    lua_dump_size(w, lua_code.constants_len() as u64)?;

    // Dump Constants
    for constant in lua_code.constants() {
        lua_dump_byte(w, constant.lua_type().bits())?;

        match *constant {
            LuaConstants::Integer(i) => lua_dump_integer(w, i)?,
            LuaConstants::Float(f) => lua_dump_float(w, f)?,
            LuaConstants::String(ref s) => lua_dump_string(w, Some(s))?,
            _ => todo!(),
        }
    }

    // Dump size of UpValues
    lua_dump_size(w, lua_code.upvalues.len() as u64)?;

    // Dump UpValues
    for (_, upv) in &lua_code.upvalues {
        w.write_all(&[upv.stack, upv.index, upv.kind.bits()])?;
    }

    // Dump size of Protos
    lua_dump_size(w, 0)?;

    // Dump Protos

    // Dump size of Debug line info
    lua_dump_size(w, 0)?;

    // Dump size of Debug abs line info
    lua_dump_size(w, 0)?;

    // Dump size of Debug loc vars
    lua_dump_size(w, 0)?;

    // Dump size of Debug upvalues
    lua_dump_size(w, 0)?;

    // Dump Debug
    Ok(())
}

#[inline]
fn lua_dump_integer(w: &mut dyn Write, n: i64) -> io::Result<()> {
    w.write_i64::<LittleEndian>(n)
}

#[inline]
fn lua_dump_float(w: &mut dyn Write, f: f64) -> io::Result<()> {
    w.write_f64::<LittleEndian>(f)
}

#[inline]
fn lua_dump_size(w: &mut dyn Write, mut data: u64) -> io::Result<()> {
    if data == 0 {
        return lua_dump_byte(w, 0x80);
    }

    let mut buf = [0u8; u64::BITS as usize / 7 + 1];
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
fn lua_dump_string(w: &mut dyn Write, opt_str: Option<&String>) -> io::Result<()> {
    match opt_str {
        Some(s) => {
            let bytes = s.as_bytes();
            lua_dump_size(w, (bytes.len() + 1) as u64)?;
            w.write_all(bytes)
        },
        None => lua_dump_size(w, 0),
    }
}

#[inline]
fn lua_dump_block() -> io::Result<()> {
    Ok(())
}

impl CompiledCode for LuaCompiledCode {
    fn get_bytes(&self, w: &mut dyn Write) -> io::Result<()> {
        Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Can't get code from single Lua function",
        ))
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
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

        let mut buf = vec![0u8; 0];
        lua_dump_size(&mut buf, 668).unwrap();
        assert_eq!(0x05, buf[0]);
        assert_eq!(0x9c, buf[1]);
    }
}
