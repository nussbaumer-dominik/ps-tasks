import {Type} from "../models/type.mjs";
import {Expr, Apply, Basic, Pairs} from "../models/expr.mjs";

class Parser {
    constructor(tokens) {
        this.tokens = tokens;
    }

    parse() {
        let result = this.parseExpression(this.tokens);
        if (this.tokens[0].type !== Type.EOF) {
            throw new Error("Unexpected token: " + this.tokens[0].text);
        }
        return result;
    }

    parseExpression(exprTokens) {
        //check for lambda
        if (exprTokens[0].type === Type.ENTITY && exprTokens[1].type === Type.LAMBDA) {
            let name = exprTokens[0].text;
            exprTokens.shift();
            exprTokens.shift();
            return Expr.createNamed(name, this.parseExpression(exprTokens));
        }

        //else check for apply
        return Expr.createApply(this.parseApply(exprTokens));
    }

    parseApply(applyTokens) {
        let basic = this.parseBasic(applyTokens);
        //check for apply
        if (applyTokens.length > 0 && applyTokens[0].type !== Type.EOF) {
            let apply = this.parseApply(applyTokens);
            return Apply.createApply(apply, basic);
        }
        return Apply.createBasic(basic);
    }

    parseBasic(basicTokens) {
        if (basicTokens[0].type === Type.EOF) {
            return null;
        }
        let basic = null;
        //check for number
        if (basicTokens[0].type === Type.NUMBER) {
            basic = Basic.createNumber(basicTokens[0].text);
            basicTokens.shift();
        } else if (basicTokens[0].type === Type.ENTITY || basicTokens[0].type === Type.OPERATION) {
            //check for name
            basic = Basic.createName(basicTokens[0].text);
            basicTokens.shift();
        } else if (basicTokens[0].type === Type.LPAREN) {
            //check for expr
            basicTokens.shift();
            //check for closing parenthesis
            let i = basicTokens.length - 1  ;
            while (basicTokens[i].type !== Type.RPAREN) {
                i--;
            }
            //split tokens at i
            let exprTokens = basicTokens.splice(0, i);
            //remove closing parenthesis
            basicTokens.shift();
            basic = Basic.createExpr(this.parseExpression(exprTokens));
        } else {
            basicTokens.shift();
        }

        //TODO: check for pairs
        //TODO: add curly brackets to the tokens

        return basic;

    }


    parsePairs() {
        //TODO implement
    }

}

export {Parser};
