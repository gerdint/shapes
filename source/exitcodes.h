#ifndef exitcodes_h
#define exitcodes_h


namespace Shapes
{
	namespace Interaction
	{
		enum ExitCode{
			EXIT_OK = 0,
			EXIT_GENERIC_ERROR = 1, EXIT_USER_ERROR = 2, EXIT_TEX_ERROR = 3, EXIT_EXTERNAL_ERROR = 4, EXIT_INVOCATION_ERROR = 5, EXIT_TOLERANCE_ERROR = 6,
			EXIT_INTERNAL_ERROR = 10, EXIT_NOT_IMPLEMENTED = 11,
			EXIT_FILE_ERROR = 20, EXIT_INPUT_FILE_ERROR = 21, EXIT_OUTPUT_FILE_ERROR = 22, EXIT_FILE_PERMISSION_ERROR = 23, EXIT_NO_DIRECTORY_ERROR = 24,
		};
	}
}


#endif
