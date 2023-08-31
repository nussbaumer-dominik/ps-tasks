class Expr {
    constructor(apply, name, expr) {
        this.apply = apply;
        this.name = name;
        this.expr = expr;
    }

    static createApply(apply) {
        return new Expr(apply, null, null);
    }

    static createNamed(name, expr) {
        return new Expr(null, name, expr);
    }
}

class Apply {
    constructor(apply, basic) {
        this.apply = apply;
        this.basic = basic;
    }

    static createBasic(basic) {
        return new Apply(null, basic);
    }

    static createApply(apply, basic) {
        return new Apply(apply, basic);
    }
}

class Basic {
    constructor(number, name, expr, pairs) {
        this.number = number;
        this.name = name;
        this.expr = expr;
        this.pairs = pairs;
    }

    static createNumber(number) {
        return new Basic(number, null, null, null);
    }

    static createName(name) {
        return new Basic(null, name, null, null);
    }

    static createExpr(expr) {
        return new Basic(null, null, expr, null);
    }

    static createPairs(pairs) {
        return new Basic(null, null, null, pairs);
    }
}

class Pairs {
    constructor(pairs, name, expr) {
        this.pairs = pairs;
        this.name = name;
        this.expr = expr;
    }

    static createSinglePair(name, expr) {
        return new Pairs(null, name, expr);
    }

    static createPairs(pairs, name, expr) {
        return new Pairs(pairs, name, expr);
    }
}

export { Expr, Apply, Basic, Pairs };
