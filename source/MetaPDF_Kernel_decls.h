#ifndef MetaPDF_Kernel_decls_h
#define MetaPDF_Kernel_decls_h

#include "refcount.h"

#include "MetaPDF_Lang_decls.h"

#include <set>


namespace MetaPDF
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

  }
}

#endif
