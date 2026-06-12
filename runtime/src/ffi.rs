use std::ffi::CStr;
use std::os::raw::c_char;

#[unsafe(no_mangle)]
pub extern "C" fn lox_compile(data: *const u8, len: usize, output_path: *const c_char) -> i32 {
    let bytes = unsafe { std::slice::from_raw_parts(data, len) };
    let path = unsafe { CStr::from_ptr(output_path) }
        .to_str()
        .unwrap_or("out.o");
    match crate::compiler::compile(bytes, path) {
        Ok(()) => 0,
        Err(e) => {
            eprintln!("compile error: {}", e);
            1
        }
    }
}
