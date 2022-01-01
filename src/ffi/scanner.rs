mod ast;
mod parser;
mod utils;

use std::ptr;

use parser::*;
use std::ffi::*;
use std::os::raw::*;

macro_rules! into_ptr {
    ($obj: expr) => {
        Box::into_raw(Box::new($obj))
    };
}

#[allow(unused_macros)]
macro_rules! from_ptr {
    ($ptr: expr) => {
        unsafe { &*$ptr }
    };
}

macro_rules! from_ptr_mut {
    ($ptr: expr) => {
        unsafe { &mut *$ptr }
    };
}

macro_rules! free_ptr {
    ($ptr: expr) => {
        if !$ptr.is_null() {
            unsafe {
                Box::from_raw($ptr);
            }
        }
    };
}

#[inline]
#[no_mangle]
pub extern "C" fn lexer_create<'input>(input: *const c_char) -> *mut StLexer<'input> {
    let input = unsafe { CStr::from_ptr(input) };
    let input = match input.to_str() {
        Ok(s) => s,
        Err(_) => return ptr::null_mut(),
    };

    into_ptr!(StLexer::new(input))
}

#[inline]
#[no_mangle]
pub extern "C" fn lexer_get_next(lexer: *mut StLexer) -> *mut LexerResult {
    let lexer = from_ptr_mut!(lexer);
    into_ptr!(lexer.next().unwrap())
}

#[inline]
#[no_mangle]
pub extern "C" fn lexer_delete(lexer: *mut StLexer) {
    free_ptr!(lexer)
}

#[inline]
#[no_mangle]
pub extern "C" fn lexer_options_create() -> *mut StLexerOptions {
    into_ptr!(StLexerOptions::default())
}
