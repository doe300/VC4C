/* 
 * Header for the public C-interface
 *
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef C_INTERFACE_H
#define C_INTERFACE_H

#ifdef __cplusplus
extern "C"
{
#endif
    
    #define LOG_DEBUG 'D'
    #define LOG_INFO 'I'
    #define LOG_WARNING 'W'
    #define LOG_ERROR 'E'
    #define LOG_SEVERE 'S'
    
    typedef struct _configuration
    {
        unsigned math_type;
        unsigned output_mode;
        char log_level;
    } configuration;
    
    #define MATH_TYPE_FAST 1
    #define MATH_TYPE_EXACT 2
    #define MATH_TYPE_STRICT 3

    #define OUTPUT_BINARY 0
    #define OUTPUT_HEX 1
    #define OUTPUT_ASSEMBLER 2

    extern const configuration DEFAULT_CONFIG;
    
    typedef struct _data_storage
    {
        unsigned is_file;
        union {
            char* data;
            char* file_name;
        };
        unsigned long data_length;
    } storage;

    int convert(const storage* in, storage* out, const configuration config, const char* options);

    typedef void(*CompilationErrorHandler)(const char* message, const unsigned length, void* userData);
    void setErrorHandler(CompilationErrorHandler errorHandler, void* userData);
    
    #define SOURCE_TYPE_UNKNOWN 0
    #define SOURCE_TYPE_OPENCL_C 1
    #define SOURCE_TYPE_LLVM_IR 2
    #define SOURCE_TYPE_SPIRV_BIN 3
    #define SOURCE_TYPE_SPIRV_TEXT 4
    #define SOURCE_TYPE_QPUASM_HEX 5
    #define SOURCE_TYPE_QPUASM_BIN 6

    int determineSourceType(const storage* in);

#ifdef __cplusplus
}
#endif

#endif /* C_INTERFACE_H */

