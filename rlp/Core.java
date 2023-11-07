package rlp;
/*------------------------------------------------------------------------------*/
import java.util.function.BiFunction;
/*------------------------------------------------------------------------------*/

public class Core
{
    /* day 1: boy do i miss Haskell */
    public sealed interface Expr permits App, TyApp, Abs, TyAbs, Var {}
    public record App(Expr f, Expr x)       implements Expr {}
    public record TyApp(Expr f, Ty x)       implements Expr {}
    public record Abs(Name n, Ty t, Expr m) implements Expr {}
    public record TyAbs(Name n, Expr m)     implements Expr {}
    public record Var(Name n)               implements Expr {}

    sealed interface Ty permits TyVar, Forall, TyFunc {}
    public record TyVar(Name n)         implements Ty {}
    public record Forall(Name n, Ty t)  implements Ty {}
    public record TyFunc(Ty a, Ty b)    implements Ty {}

    public final static class Name
    {
        public final String s;
        public Name(String s)
        {
            // TODO: verify that `s` is a valid identifier
            this.s = s;
        }
    }

    public static String showExpr(Expr e)
    {
        return showExprP(0, e);
    }

    static String showExprP(int p, Expr e)
    {
        return switch(e)
        {
            /* prec: 1 */
            case App a ->
                wrap(p, 1, showExprP(2, a.f) + " " + showExprP(2, a.x));

            /* prec: 1 */
            case TyApp a ->
                wrap(p, 1, showExprP(2, a.f) + " " + showTyP(2, a.x));

            /* prec: 0 */
            case Abs a ->
                wrap(p, 0, String.format
                    ( "λ(%s:%s).%s"
                    , a.n.s
                    , showTyP(0, a.t)
                    , showExprP(0, a.m)));

            /* prec: 0 */
            case TyAbs a ->
                wrap(p, 0, String.format
                    ( "Λ%s.%s"
                    , a.n.s
                    , showExprP(0, a.m)));

            case Var a ->
                a.n.s;
        };
    }

    static String showTy(Ty t)
    {
        return showTyP(0, t);
    }

    static String showTyP(int p, Ty t)
    {
        return switch(t)
        {
            case TyVar a   -> a.n.s;
            case Forall a  -> "";
            case TyFunc a  -> "";
        };
    }

    static String wrap(int p, int n, String s)
    {
        return (p <= n) ? s : "(" + s + ")";
    }
}

