import distutils.ccompiler
import distutils.sysconfig
import tempfile
import cffi
def ffibuilder():
    builder = cffi.FFI()
    builder.set_source('deltachat.capi', '\n            #include <deltachat/deltachat.h>\n            const char * dupstring_helper(const char* string)\n            {\n                return strdup(string);\n            }\n            int dc_get_event_signature_types(int e)\n            {\n                int result = 0;\n                if (DC_EVENT_DATA1_IS_STRING(e))\n                    result |= 1;\n                if (DC_EVENT_DATA2_IS_STRING(e))\n                    result |= 2;\n                if (DC_EVENT_RETURNS_STRING(e))\n                    result |= 4;\n                if (DC_EVENT_RETURNS_INT(e))\n                    result |= 8;\n                return result;\n            }\n        ', libraries=['deltachat'])
    builder.cdef('\n        typedef int... time_t;\n        void free(void *ptr);\n        extern const char * dupstring_helper(const char* string);\n        extern int dc_get_event_signature_types(int);\n    ')
    cc = distutils.ccompiler.new_compiler(force=True)
    distutils.sysconfig.customize_compiler(cc)
    with tempfile.NamedTemporaryFile(mode='w', suffix='.h') as src_fp:
        src_fp.write('#include <deltachat/deltachat.h>')
        src_fp.flush()
        with tempfile.NamedTemporaryFile(mode='r') as dst_fp:
            cc.preprocess(source=src_fp.name, output_file=dst_fp.name, macros=[('PY_CFFI', '1')])
            builder.cdef(dst_fp.read())
    builder.cdef('\n        extern "Python" uintptr_t py_dc_callback(\n            dc_context_t* context,\n            int event,\n            uintptr_t data1,\n            uintptr_t data2);\n    ')
    return builder
if __name__ == '__main__':
    import os.path
    pkgdir = os.path.join(os.path.dirname(__file__), '..')
    builder = ffibuilder()
    builder.compile(tmpdir=pkgdir, verbose=True)