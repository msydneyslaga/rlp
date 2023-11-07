package rlp.rlpc;
/*----------------------------------------------------------------------------*/
import rlp.Core;
/*----------------------------------------------------------------------------*/

class RLPC
{
	public static void main(String[] argv)
	{
		// final Core.Expr e = new Core.App(new Core.Var(new Core.Name("f")), new Core.Var(new Core.Name("x")));
		final Core.Expr e =
			new Core.App
				( new Core.Abs
					( new Core.Name("x")
					, new Core.VarTy(new Core.Name("α"))
					, new Core.Var(new Core.Name("x"))
					)
				, new Core.Var(new Core.Name("y")));
		System.out.println(Core.showExpr(e));
	}
}

