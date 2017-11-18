/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TMU.h"

#include "../InstructionWalker.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::periphery;

/*
 * XXX the TMU can queue up to 2? load-requests per QPU, could be used to optimize/reorder TMU loads
 *
 * see i.e. https://github.com/raspberrypi/userland/blob/master/host_applications/linux/apps/hello_pi/hello_fft/qasm/gpu_fft.qinc (.macro load_tw, .macro read_lin/.macro load_lin)
 */

InstructionWalker periphery::insertGeneralReadTMU(InstructionWalker it, const Value& dest, const Value& addr)
{
	/*
	 * Mutex lock?
	 * hello_fft (FFT2) from the Raspbian userland-repository doesn't lock its TMU requests so for now, we don't either
	 */

	//"General-memory lookups are performed by writing to just the s-parameter, using the absolute memory address" (page 41)
	//1) write address to TMU_S register
	it.emplace(new intermediate::MoveOperation(TMU_GENERAL_READ_ADDRESS, addr));
	it.nextInBlock();
	//2) trigger loading of TMU
	it.emplace(new intermediate::Nop(intermediate::DelayType::WAIT_TMU));
	it->setSignaling(SIGNAL_LOAD_TMU0);
	it.nextInBlock();
	//3) read value from R4
	it.emplace(new intermediate::MoveOperation(dest, TMU_READ_REGISTER));
	it.nextInBlock();
	return it;
}

InstructionWalker periphery::insertReadTMU(Method& method, InstructionWalker it, const Value& image, const Value& dest, const Value& xCoord, const Optional<Value>& yCoord)
{
	if(!image.hasType(ValueType::LOCAL))
		throw CompilationError(CompilationStep::GENERAL, "Cannot access image-configuration for non-local image", image.to_string());
	const Global* imageConfig = method.findGlobal(ImageType::toImageConfigurationName(image.local->name));
	if(imageConfig == nullptr)
		throw CompilationError(CompilationStep::GENERAL, "Failed to find the image-configuration for", image.to_string());
	if(!xCoord.type.isFloatingType())
		throw CompilationError(CompilationStep::GENERAL, "Can only read with floating-point coordinates in the x-axis", xCoord.to_string());
	if(yCoord.hasValue && !yCoord.get().type.isFloatingType())
		throw CompilationError(CompilationStep::GENERAL, "Can only read with floating-point coordinates in the y-axis", yCoord.to_string());

	// 1. set the UNIFORM pointer to point to the configurations for the image about to be read
	it.emplace(new intermediate::MoveOperation(Value(REG_UNIFORM_ADDRESS, TYPE_INT32.toVectorType(16).toPointerType()), imageConfig->createReference()));
	it.nextInBlock();
	// 2. need to wait 2 instructions for UNIFORM-pointer to be changed
	it.emplace(new intermediate::Nop(intermediate::DelayType::WAIT_UNIFORM));
	it.nextInBlock();
	it.emplace(new intermediate::Nop(intermediate::DelayType::WAIT_UNIFORM));
	it.nextInBlock();
	// 3. write the TMU addresses
	if(yCoord.hasValue)
	{
		it.emplace(new intermediate::MoveOperation(TMU_COORD_T_REGISTER, yCoord));
		it.nextInBlock();
	}
	else
	{
		//for 1D-images, we only have an x-coordinate, but if we only write the TMU_S register, general TMU lookup is used!
		//so we write a dummy y-coordinate with a value of zero, to select the first row
		it.emplace(new intermediate::MoveOperation(TMU_COORD_T_REGISTER, INT_ZERO));
		it.nextInBlock();
	}
	it.emplace(new intermediate::MoveOperation(TMU_COORD_S_REGISTER, xCoord));
	it.nextInBlock();
	// 4. trigger loadtmu
	it.emplace(new intermediate::Nop(intermediate::DelayType::WAIT_TMU));
	it->setSignaling(SIGNAL_LOAD_TMU0);
	it.nextInBlock();
	// 5. read from r4 (stalls 9 to 20 cycles)
	it.emplace(new intermediate::MoveOperation(dest, TMU_READ_REGISTER));
	it.nextInBlock();
	// 6. TODO reset UNIFORM pointer? for next work-group iteration, or disable when used with images?

	return it;
}
