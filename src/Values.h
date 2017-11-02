/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VALUES_H
#define VALUES_H

#include "Types.h"
#include "performance.h"
#include "Bitfield.h"

namespace vc4c
{
	enum class RegisterFile
	{
		NONE = 0, PHYSICAL_A = 1, PHYSICAL_B = 2, PHYSICAL_ANY = 3, ACCUMULATOR = 4, ANY = 7
	};

	std::string toString(const RegisterFile file);
	bool isFixed(const RegisterFile file);

	struct Register
	{
		RegisterFile file;
		std::size_t num;

		explicit Register();
		constexpr Register(const RegisterFile file, const std::size_t num) : file(file), num(num) {};

		bool isGeneralPurpose() const;
		std::string to_string(bool specialNames, bool readAccess = true) const;
		int getAccumulatorNumber() const;

		bool operator<(const Register& right) const;
		bool operator>(const Register& right) const;
		bool operator==(const Register& right) const;
		bool operator!=(const Register& right) const
		{
			return !(*this == right);
		}

		bool isAccumulator() const;
		bool isVertexPipelineMemory() const;
		bool isSpecialFunctionsUnit() const;
		bool hasSideEffectsOnRead() const;
		bool hasSideEffectsOnWrite() const;

		bool isReadable() const;
		bool isWriteable() const;

		static constexpr int INVALID_ACCUMULATOR { -1 };
	};

	static constexpr Register REG_UNIFORM { RegisterFile::PHYSICAL_ANY, 32 };
	static constexpr Register REG_ACC0 { RegisterFile::ACCUMULATOR, 32 };
	static constexpr Register REG_ACC1 { RegisterFile::ACCUMULATOR, 33 };
	static constexpr Register REG_ACC2 { RegisterFile::ACCUMULATOR, 34 };
	static constexpr Register REG_ACC3 { RegisterFile::ACCUMULATOR, 35 };
	static constexpr Register REG_SFU_OUT { RegisterFile::ACCUMULATOR, 36 };
	/*
	 * "TMU read generates:
	 *     9 clock stalls when it reads from TMU cache.
	 *     12 clock stalls when it reads from V3D L2 cache.
	 *     20 clock stalls when it reads directly from memory."
	 * - see https://www.raspberrypi.org/forums/viewtopic.php?p=1143940#p1144081
	 */
	static constexpr Register REG_TMU_OUT { RegisterFile::ACCUMULATOR, 36 };
	static constexpr Register REG_ACC5 { RegisterFile::ACCUMULATOR, 37 };
//distributes the value from the first element of the quad (0, 4, 8, 12) to all other elements of this quad
	static constexpr Register REG_REPLICATE_QUAD { RegisterFile::PHYSICAL_A, 37 };
//distributes the value from the first element to all 15 other elements
	static constexpr Register REG_REPLICATE_ALL { RegisterFile::PHYSICAL_B, 37 };

//read-only
	static constexpr Register REG_ELEMENT_NUMBER { RegisterFile::PHYSICAL_A, 38 };
	static constexpr Register REG_QPU_NUMBER { RegisterFile::PHYSICAL_B, 38 };
//write-only
	static constexpr Register REG_HOST_INTERRUPT { RegisterFile::PHYSICAL_ANY, 38 };
	static constexpr Register REG_NOP { RegisterFile::PHYSICAL_ANY, 39 };

	/*
	 * "The uniform base pointer can be written (from SIMD element 0) by the processor to reset the stream,
	 * there must be at least two nonuniform-accessing instructions following a pointer change before uniforms can be accessed once more."
	 * - Broadcom specification, page 22
	 */
	static constexpr Register REG_UNIFORM_ADDRESS { RegisterFile::PHYSICAL_ANY, 40 };

//VPM I/O, see specification section 7
	static constexpr Register REG_VPM_IO { RegisterFile::PHYSICAL_ANY, 48 };
//read-only
	static constexpr Register REG_VPM_IN_BUSY { RegisterFile::PHYSICAL_A, 49 };
	static constexpr Register REG_VPM_OUT_BUSY { RegisterFile::PHYSICAL_B, 49 };
//write-only
	static constexpr Register REG_VPM_IN_SETUP { RegisterFile::PHYSICAL_A, 49 };
	static constexpr Register REG_VPM_OUT_SETUP { RegisterFile::PHYSICAL_B, 49 };
//read-only
	/*
	 * "VPM read generates 5 clock stalls between "VPM generic block read setup" and the first VPM_READ read."
	 * - see https://www.raspberrypi.org/forums/viewtopic.php?p=1143940#p1144081 and http://imrc.noip.me/blog/vc4/QV56/
	 *
	 * -> so this blocks always for at least 5 instructions (if no other instructions are inserted in between)
	 */
	static constexpr Register REG_VPM_IN_WAIT { RegisterFile::PHYSICAL_A, 50 };
	static constexpr Register REG_VPM_OUT_WAIT { RegisterFile::PHYSICAL_B, 50 };
//write-only
	static constexpr Register REG_VPM_IN_ADDR { RegisterFile::PHYSICAL_A, 50 };
	static constexpr Register REG_VPM_OUT_ADDR { RegisterFile::PHYSICAL_B, 50 };

//Mutex
	static constexpr Register REG_MUTEX { RegisterFile::PHYSICAL_ANY, 51 };

//Special Functions Unit
	static constexpr Register REG_SFU_RECIP { RegisterFile::PHYSICAL_ANY, 52 };
	static constexpr Register REG_SFU_RECIP_SQRT { RegisterFile::PHYSICAL_ANY, 53 };
	static constexpr Register REG_SFU_EXP2 { RegisterFile::PHYSICAL_ANY, 54 };
	static constexpr Register REG_SFU_LOG2 { RegisterFile::PHYSICAL_ANY, 55 };

//Texture Memory Unit
//since we leave TMU auto-swap enabled, we only need to write to TMU0

	/*
	 * "General-memory lookups are performed by writing to just the ‘s’ parameter, using the absolute memory address.
	 * In this case no uniform is read. General-memory lookups always return a 32-bit value, and the bottom two bits of the address are ignored."
	 * - Broadcom specification, page 41
	 *
	 * According to several sources, when writing 16 different addresses via the 16 SIMD-elements, the TMU returns values read from the 16 addresses,
	 * not just the one for element 0. (e.g. https://www.raspberrypi.org/forums/viewtopic.php?p=1143940#p1143940 and http://www.aholme.co.uk/GPU_FFT/Main.htm#Parallelism)
	 */
	static constexpr Register REG_TMU_ADDRESS { RegisterFile::PHYSICAL_ANY, 56 };
	static constexpr Register REG_TMU_COORD_S_U_X { RegisterFile::PHYSICAL_ANY, 56 };
	static constexpr Register REG_TMU_COORD_T_V_Y { RegisterFile::PHYSICAL_ANY, 57 };
	static constexpr Register REG_TMU_COORD_R_BORDER_COLOR { RegisterFile::PHYSICAL_ANY, 58 };
	static constexpr Register REG_TMU_COORD_B_LOD_BIAS { RegisterFile::PHYSICAL_ANY, 59 };

	enum class LiteralType
	{
		INTEGER, REAL, BOOL
	};

	struct Literal
	{
		int64_t integer;
		LiteralType type;

		Literal(const int64_t integer);
		explicit Literal(const uint64_t integer);
		Literal(const double real);
		Literal(const bool flag);

		Literal(const Literal&) = default;
		Literal(Literal&&) = default;

		Literal& operator=(const Literal&) = default;
		Literal& operator=(Literal&&) = default;

		bool operator==(const Literal& other) const;

		const std::string to_string() const;
		bool isTrue() const;
		double real() const;

		uint32_t toImmediate() const;
	};

	/*!
	 * The 6 bit small immediate field encodes either an immediate integer/float value used in place of the register file b input,
	 * or a vector rotation to apply to the mul ALU output, according to Table 5.
	 *
	 * page 29
	 */
	struct SmallImmediate : public InstructionPart
	{
		constexpr SmallImmediate(const unsigned char val) : InstructionPart(val) {};

		std::string toString() const;

		//the "real" values being loaded with this small immediate

		//0 - 15 - use immediate values 0 to 15
		//16 - 31 - use immediate values -16 to -1
		Optional<char> getIntegerValue() const;
		//32 - 29 - use immediate values 1.0, 2.0, 4.0, ..., 128.0
		//40 - 47 - use immediate values 1/256, 1/128, ..., 1/2
		Optional<float> getFloatingValue() const;
		//48 - multiplication output vector rotation is taken from accumulator r5, element 0, bits [3:0] (lower half)
		//49 - 63 - multiplication output vector rotated by 1 - 15 upwards (so element 0 moves to element 1 - 15)
		bool isVectorRotation() const;
		Optional<unsigned char> getRotationOffset() const;

		Optional<Literal> toLiteral() const;

		static SmallImmediate fromRotationOffset(unsigned char offset);
	};

	constexpr SmallImmediate VECTOR_ROTATE_R5{48};

	enum class ValueType
	{
		LITERAL, LOCAL,  //also contains labels
		REGISTER,
		CONTAINER,
		UNDEFINED,
		//literal values passed by the parsers are either converted to load-instructions or small immediate values
		SMALL_IMMEDIATE
	};

	struct Value;

	struct ContainerValue
	{
		std::vector<Value> elements;

		/*
		 * Determines whether all elements of this container have the same value.
		 * If the parameter is set, only this value is accepted
		 */
		bool isAllSame(const Optional<Literal>& value = { false, false }) const;

		/*
		 * Determines whether all element-values correspond to their element number,  e.g. i32 1, i32 2, ...
		 * if the parameter is set to true, an initial offset is allowed, e.g. i32 5, i32 6, ...
		 */
		bool isElementNumber(bool withOffset = false) const;
	};

	class Local;

	struct Value
	{
		union
		{
			Literal literal;
			Register reg;
			Local* local;
			SmallImmediate immediate;
		};
		//XXX or rewrite and use pointer?
		DataType type;
		ValueType valueType;
		ContainerValue container;

		Value(const Literal& lit, const DataType& type);
		Value(const Register& reg, const DataType& type);
		Value(const ContainerValue& container, const DataType& type);
		Value(const Value& val);
		Value(Value&& val);
		Value(const Local* local, const DataType& type);
		Value(const DataType& type);
		Value(const SmallImmediate& immediate, const DataType& type);

		Value& operator=(const Value& right) = default;
		Value& operator=(Value&& right) = default;

		bool operator==(const Value& other) const;
		inline bool operator!=(const Value& other) const
		{
			return !(*this == other);
		}

		Value getCompoundPart(int index) const;
		bool hasType(const ValueType type) const;
		bool hasLocal(const Local* local) const;
		bool hasRegister(const Register& reg) const;
		bool hasLiteral(const Literal& lit) const;
		bool hasImmediate(const SmallImmediate& immediate) const;

		bool isUndefined() const;
		bool isZeroInitializer() const;
		bool isLiteralValue() const;

		std::string to_string(const bool writeAccess = false, bool withLiterals = false) const;

		bool isWriteable() const;
		bool isReadable() const;
		const Value& assertWriteable() const;
		Value& assertWriteable();
		const Value& assertReadable() const;
		Value& assertReadable();

		static Value createZeroInitializer(const DataType& type);
	};

	const Value BOOL_TRUE(Literal(true), TYPE_BOOL);
	const Value BOOL_FALSE(Literal(false), TYPE_BOOL);
	const Value INT_ZERO(Literal(static_cast<int64_t>(0)), TYPE_INT8);
	const Value INT_ONE(Literal(static_cast<int64_t>(1)), TYPE_INT8);
	const Value FLOAT_ZERO(Literal(0.0), TYPE_FLOAT);
	const Value FLOAT_ONE(Literal(1.0), TYPE_FLOAT);
	const Value FLOAT_INF(Literal(static_cast<uint64_t>(0x7F700000)), TYPE_FLOAT);
	const Value FLOAT_NAN(Literal(static_cast<uint64_t>(0x7FC00000)), TYPE_FLOAT);
	const Value UNDEFINED_VALUE(TYPE_UNKNOWN);
	const Optional<Value> NO_VALUE(false, UNDEFINED_VALUE);

	const Value UNIFORM_REGISTER(REG_UNIFORM, TYPE_INT32.toVectorType(16));
	const Value NOP_REGISTER(REG_NOP, TYPE_UNKNOWN);
	const Value ELEMENT_NUMBER_REGISTER(REG_ELEMENT_NUMBER, TYPE_INT8.toVectorType(16));
	const Value ROTATION_REGISTER(REG_ACC5, TYPE_INT8);
	const Value MUTEX_REGISTER(REG_MUTEX, TYPE_BOOL);

	template<>
	struct hash<Value> : public std::hash<std::string>
	{
		size_t operator()(const Value& ) const noexcept;
	};
} /* namespace vc4c */

#endif /* VALUES_H */
