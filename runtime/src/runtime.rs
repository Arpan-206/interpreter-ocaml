// runtime.rs — LoxValue representation and memory management
//
// Every Lox value is a heap-allocated tagged struct. We leak memory for now
// but the layout is GC-ready: the tag tells a future collector what to trace,
// and all pointers live in the payload.

use std::collections::HashMap;

/// Value tags — u64 so the struct is uniformly sized
pub const TAG_NIL: u64 = 0;
pub const TAG_BOOL: u64 = 1;
pub const TAG_NUM: u64 = 2;
pub const TAG_STRING: u64 = 3;
pub const TAG_FUN: u64 = 4;
pub const TAG_CLASS: u64 = 5;
pub const TAG_INSTANCE: u64 = 6;

/// The uniform value representation passed across the FFI boundary and
/// through all compiled Lox code. 16 bytes, always heap-allocated.
///
/// payload interpretation by tag:
///   NIL      — unused
///   BOOL     — 0 or 1 cast to u64
///   NUM      — f64 bits reinterpreted as u64
///   STRING   — *mut LoxString cast to u64
///   FUN      — *mut LoxFun cast to u64
///   CLASS    — *mut LoxClass cast to u64
///   INSTANCE — *mut LoxInstance cast to u64
#[repr(C)]
pub struct LoxValue {
    pub tag: u64,
    pub payload: u64,
}

/// Heap-allocated string. The Vec owns the bytes.
#[repr(C)]
pub struct LoxString {
    pub len: usize,
    pub data: *mut u8, // owned, allocated with Box<[u8]>
}

/// A compiled Lox function. fn_ptr is a raw pointer to JIT-compiled code.
/// arity and name are kept for runtime error messages.
#[repr(C)]
pub struct LoxFun {
    pub name: *mut LoxString,
    pub arity: u32,
    pub fn_ptr: *const u8, // JIT code pointer — called via transmute
}

/// A Lox class. methods maps method name → LoxFun pointer.
/// superclass is None for base classes.
#[repr(C)]
pub struct LoxClass {
    pub name: *mut LoxString,
    pub methods: *mut HashMap<String, *mut LoxFun>, // Box-owned
    pub superclass: *mut LoxClass,                  // null = no superclass
}

/// A Lox instance. fields is a heap-allocated map.
#[repr(C)]
pub struct LoxInstance {
    pub class: *mut LoxClass,
    pub fields: *mut HashMap<String, *mut LoxValue>, // Box-owned
}

// ── Allocation helpers ────────────────────────────────────────────────────

pub fn alloc_nil() -> *mut LoxValue {
    Box::into_raw(Box::new(LoxValue {
        tag: TAG_NIL,
        payload: 0,
    }))
}

pub fn alloc_bool(b: bool) -> *mut LoxValue {
    Box::into_raw(Box::new(LoxValue {
        tag: TAG_BOOL,
        payload: b as u64,
    }))
}

pub fn alloc_num(f: f64) -> *mut LoxValue {
    Box::into_raw(Box::new(LoxValue {
        tag: TAG_NUM,
        payload: f.to_bits(),
    }))
}

pub fn alloc_string(s: &str) -> *mut LoxValue {
    let mut bytes = s.as_bytes().to_vec().into_boxed_slice();
    let lox_str = Box::into_raw(Box::new(LoxString {
        len: bytes.len(),
        data: bytes.as_mut_ptr(),
    }));
    std::mem::forget(bytes);
    Box::into_raw(Box::new(LoxValue {
        tag: TAG_STRING,
        payload: lox_str as u64,
    }))
}

pub fn alloc_instance(class: *mut LoxClass) -> *mut LoxValue {
    let inst = Box::into_raw(Box::new(LoxInstance {
        class,
        fields: Box::into_raw(Box::new(HashMap::new())),
    }));
    Box::into_raw(Box::new(LoxValue {
        tag: TAG_INSTANCE,
        payload: inst as u64,
    }))
}

// ── Value helpers used by JIT-emitted code ────────────────────────────────

/// Truthy test matching Lox semantics: nil and false are falsy, everything else truthy.
#[unsafe(no_mangle)]
pub extern "C" fn lox_is_truthy(v: *mut LoxValue) -> u64 {
    let v = unsafe { &*v };
    match v.tag {
        TAG_NIL => 0,
        TAG_BOOL => v.payload,
        _ => 1,
    }
}

/// Print a LoxValue to stdout, matching the tree-walk interpreter's output format.
#[unsafe(no_mangle)]
pub extern "C" fn lox_print(v: *mut LoxValue) {
    let v = unsafe { &*v };
    match v.tag {
        TAG_NIL => println!("nil"),
        TAG_BOOL => println!("{}", v.payload != 0),
        TAG_NUM => {
            let f = f64::from_bits(v.payload);
            if f.fract() == 0.0 && f.abs() < 1e15 {
                println!("{:.1}", f);
            } else {
                println!("{}", f);
            }
        }
        TAG_STRING => {
            let s = unsafe { &*(v.payload as *mut LoxString) };
            let bytes = unsafe { std::slice::from_raw_parts(s.data, s.len) };
            println!("{}", std::str::from_utf8(bytes).unwrap_or("<invalid utf8>"));
        }
        TAG_FUN => {
            let f = unsafe { &*(v.payload as *mut LoxFun) };
            let name = unsafe { &*(f.name) };
            let bytes = unsafe { std::slice::from_raw_parts(name.data, name.len) };
            println!("<fn {}>", std::str::from_utf8(bytes).unwrap_or("?"));
        }
        TAG_CLASS => {
            let c = unsafe { &*(v.payload as *mut LoxClass) };
            let name = unsafe { &*(c.name) };
            let bytes = unsafe { std::slice::from_raw_parts(name.data, name.len) };
            println!("{}", std::str::from_utf8(bytes).unwrap_or("?"));
        }
        TAG_INSTANCE => {
            let inst = unsafe { &*(v.payload as *mut LoxInstance) };
            let class = unsafe { &*(inst.class) };
            let name = unsafe { &*(class.name) };
            let bytes = unsafe { std::slice::from_raw_parts(name.data, name.len) };
            println!("{} instance", std::str::from_utf8(bytes).unwrap_or("?"));
        }
        _ => println!("<unknown>"),
    }
}

/// Equality comparison matching Lox semantics.
#[unsafe(no_mangle)]
pub extern "C" fn lox_equal(a: *mut LoxValue, b: *mut LoxValue) -> u64 {
    let a = unsafe { &*a };
    let b = unsafe { &*b };
    match (a.tag, b.tag) {
        (TAG_NIL, TAG_NIL) => 1,
        (TAG_BOOL, TAG_BOOL) => (a.payload == b.payload) as u64,
        (TAG_NUM, TAG_NUM) => {
            let fa = f64::from_bits(a.payload);
            let fb = f64::from_bits(b.payload);
            (fa == fb) as u64
        }
        (TAG_STRING, TAG_STRING) => {
            let sa = unsafe { &*(a.payload as *mut LoxString) };
            let sb = unsafe { &*(b.payload as *mut LoxString) };
            if sa.len != sb.len {
                return 0;
            }
            let ba = unsafe { std::slice::from_raw_parts(sa.data, sa.len) };
            let bb = unsafe { std::slice::from_raw_parts(sb.data, sb.len) };
            (ba == bb) as u64
        }
        _ => 0,
    }
}
