  class PathStatement : public Statement
  {
    Ast::Expression * expr;
  protected:
    const Lang::MultiPath2D * path( );
  public:
    PathStatement( Ast::Expression * _expr );
    virtual ~PathStatement( );
  };

  class Stroke : public PathStatement
  {
  public:
    Stroke( Ast::Expression * _expr );
    virtual ~Stroke( );
    virtual void writeTo( std::ostream & os, GraphicsState * metaState, GraphicsState * pdfState, Environment * env );
  };

  class Fill : public PathStatement
  {
  public:
    Fill( Ast::Expression * _expr );
    virtual ~Fill( );
    virtual void writeTo( std::ostream & os, GraphicsState * metaState, GraphicsState * pdfState, Environment * env );
  };

