/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_COMPILATION_DATA
#define VC4C_COMPILATION_DATA

#include "../Optional.h"
#include "Precompiler.h"
#include "tool_paths.h"

#include <array>
#include <fstream>
#include <memory>
#include <sstream>
#include <vector>

namespace llvm
{
    class Module;
} // namespace llvm

namespace vc4c
{
    std::string readIntoString(std::istream& stream);

    class CompilationDataPrivate : private NonCopyable
    {
    public:
        virtual ~CompilationDataPrivate() noexcept;

        virtual SourceType getType() const noexcept = 0;
        virtual Optional<std::string> getFilePath() const = 0;
        virtual std::string to_string() const = 0;

        virtual void readInto(std::ostream& out) const = 0;
        virtual void writeFrom(std::istream& in) = 0;

        /**
         * Returns a stream reading from the underlying buffer or a copy of it.
         */
        virtual std::unique_ptr<std::istream> readStream() const
        {
            auto tmp = std::make_unique<std::stringstream>();
            readInto(*tmp);
            return tmp;
        }

        /**
         * Returns a stream writing to the underlying buffer, or NULL of no such stream exists.
         */
        virtual std::unique_ptr<std::ostream> writeStream()
        {
            return nullptr;
        }

        virtual Optional<std::vector<uint8_t>> getRawData() const
        {
            return {};
        }
    };

    namespace precompilation
    {
        template <SourceType Type>
        struct TypedCompilationData : public CompilationDataPrivate
        {
            ~TypedCompilationData() noexcept override = default;

            SourceType getType() const noexcept override
            {
                return Type;
            }
        };

        using OpenCLData = TypedCompilationData<SourceType::OPENCL_C>;
        using LLVMIRData = TypedCompilationData<SourceType::LLVM_IR_BIN>;
        using LLVMIRTextData = TypedCompilationData<SourceType::LLVM_IR_TEXT>;
        using SPIRVData = TypedCompilationData<SourceType::SPIRV_BIN>;
        using SPIRVTextData = TypedCompilationData<SourceType::SPIRV_TEXT>;

        template <SourceType Type>
        struct FileCompilationData : TypedCompilationData<Type>
        {
            explicit FileCompilationData(const std::string& file) : filePath(file) {}
            ~FileCompilationData() noexcept override = default;

            Optional<std::string> getFilePath() const override
            {
                return filePath;
            }

            std::string to_string() const override
            {
                return filePath;
            }

            void readInto(std::ostream& out) const override
            {
                std::ifstream fis{filePath};
                out << fis.rdbuf();
            }

            void writeFrom(std::istream& in) override
            {
                std::ofstream fos{filePath};
                fos << in.rdbuf();
            }

            std::unique_ptr<std::istream> readStream() const override
            {
                return std::make_unique<std::ifstream>(filePath);
            }

            std::unique_ptr<std::ostream> writeStream() override
            {
                return std::make_unique<std::ofstream>(filePath);
            }

            std::string filePath;
        };

        template <SourceType Type>
        struct TemporaryFileCompilationData : FileCompilationData<Type>
        {
            explicit TemporaryFileCompilationData(TemporaryFile&& file = TemporaryFile{}) :
                FileCompilationData<Type>(file.fileName), tempFile(std::move(file))
            {
            }

            ~TemporaryFileCompilationData() noexcept override = default;

            TemporaryFile tempFile;
        };

        template <SourceType Type>
        struct RawCompilationData : TypedCompilationData<Type>
        {
            explicit RawCompilationData(const std::string& name) : name(name) {}
            explicit RawCompilationData(std::vector<uint8_t>&& data, const std::string& name = "") :
                data(std::move(data)), name(name)
            {
            }
            explicit RawCompilationData(std::istream& s, const std::string& name = "") : name(name)
            {
                writeFrom(s);
            }
            ~RawCompilationData() noexcept override = default;

            Optional<std::string> getFilePath() const override
            {
                return {};
            }

            std::string to_string() const override
            {
                if(!name.empty())
                    return "(buffer '" + name + "')";
                return "(buffer of " + std::to_string(data.size()) + " bytes)";
            }

            void readInto(std::ostream& out) const override
            {
                out.write(reinterpret_cast<const char*>(data.data()), data.size());
            }

            void writeFrom(std::istream& in) override
            {
                std::string tmp = readIntoString(in);
                data.assign(tmp.begin(), tmp.end());
            }

            Optional<std::vector<uint8_t>> getRawData() const override
            {
                return data;
            }

            std::vector<uint8_t> data;
            std::string name;
        };

    } /* namespace precompilation */
} /* namespace vc4c */

#endif /* VC4C_COMPILATION_DATA */
