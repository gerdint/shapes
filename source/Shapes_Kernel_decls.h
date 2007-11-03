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
