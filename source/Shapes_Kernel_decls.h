/* This file is part of Shapes.
 *
 * Shapes is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * Shapes is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Shapes.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2008 Henrik Tidefelt
 */

#ifndef Shapes_Kernel_decls_h
#define Shapes_Kernel_decls_h

#include "refcount.h"

#include "Shapes_Lang_decls.h"

#include <set>


namespace Shapes
{
	namespace Kernel
	{

		typedef RefCountPtr< const Lang::Value > ValueRef;

		class Variable;
		typedef RefCountPtr< Variable > VariableHandle;

		class State;
		typedef State * StateHandle;

		class Continuation;
		typedef RefCountPtr< Continuation > ContRef;

		class Environment;
		typedef Environment * PassedEnv;
		typedef std::set< Environment * > GCMarkedSet;

		class DynamicEnvironment;
		typedef RefCountPtr< DynamicEnvironment > PassedDyn;

		class SystemDynamicVariables;
		typedef size_t DynamicEnvironmentKeyType;

		class EvalState;
		class PageContentStates;
		class GraphicsState;
		class FacetState;
		class TextState;

		class Formals;
		class EvaluatedFormals;

		class StoreVariableContinuation;
		class StmtStoreVariableContinuation;

		class Arguments;

		class CallContInfo;
		class CodeBracketContInfo;

		class Thunk;

		class Warm;
		class WarmGroup2D;
		class WarmRandomDevice;

		class PolarHandlePromise;

		class TeXLabelManager;

		class DebugLog;

		void registerGlobals( Kernel::Environment * env );
		void registerDynamic( Kernel::Environment * env );
		void registerHot( Kernel::Environment * env );
		void registerClasses( Kernel::Environment * env );

		void registerCore_elem( Kernel::Environment * env );
		void registerCore_point( Kernel::Environment * env );
		void registerCore_path( Kernel::Environment * env );
		void registerCore_draw( Kernel::Environment * env );
		void registerCore_construct( Kernel::Environment * env );
		void registerCore_font( Kernel::Environment * env );
		void registerCore_misc( Kernel::Environment * env );
		void registerCore_state( Kernel::Environment * env );
		void registerCore_annotation( Kernel::Environment * env );
	}
}

#endif
