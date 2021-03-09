
// The maximum number of arguments that can be passed to a method.
//
// Limitation from instruction set.
pub const MAX_PARAMETERS: usize = 16;

/// Maximum length of a method name, excluding the
/// rest of the signature.
/// 
/// This limit is arbitrary. A known size makes
/// parsing easier in Wren's C code.
pub const MAX_METHOD_NAME: usize = 64;
