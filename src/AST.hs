data Literal
  = LitStr String
  | LitNum Integer

newtype ModuleName = [Name]
data Name
  = QualifiedName ModuleName String
  | UnqualifiedName String

data Expr
  = LiteralExpr Literal
  | CallExpr Expr [Expr]
  | LoopExpr Expr Block
  | ConditionalExpr Expr Block Block

data Import = Import ModuleName
data Var = String -- maybe newtype UnqualifiedName?

data Parameter = Parameter String -- ^
data ParameterList = [Parameter]
data Fn = Fn String ParameterList -- ^

data Decl = ImportDecl Import | VarDecl Var | FnDecl Fn

data Line = LineExpr Expr | LineDecl Decl
data Block = [Line] -- todo takewhile isDecl + error if contains LineDecl in _2
