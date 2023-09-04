import {Type} from "../models/type.mjs";
import {Expr, Basic, Value, FunctionCall, Assign} from "../models/expr.mjs";

function parse(tokens) {
    const result = [];
    while (tokens.length > 0) {
        if (tokens[0].type === Type.EOL) {
            tokens.shift();
            continue;
        }
        result.push(parseBasic(tokens))
    }
    return result;
}

function parseBasic(exprTokens) {
    //check for lambda
    if (exprTokens[0].type === Type.ENTITY && exprTokens.length > 1 && exprTokens[1].type === Type.ASSIGN) {
        let name = exprTokens[0].text;
        exprTokens.shift();
        exprTokens.shift();
        //TODO get scoped expression
        const expression = [exprTokens[0]];
        exprTokens.shift();
        return Basic.createAssignment(new Assign(name, parseBasic(expression)));
    }

    //else check for apply
    return Basic.createValue(parseValue(exprTokens));
}

function parseValue(basicTokens) {
    let basic = null;
    //check for number
    if (basicTokens[0].type === Type.NUMBER) {
        basic = Value.createNumber(basicTokens[0].text);
        basicTokens.shift();
    }
    else if (basicTokens[0].type === Type.ENTITY  && basicTokens[1]?.type === Type.LCURLY) {
        const name = basicTokens[0].text;
        //check for expr
        basicTokens.shift();
        basicTokens.shift();
        //check for closing parenthesis
        let i = basicTokens.length - 1;
        while (basicTokens[i].type !== Type.RCURLY) {
            i--;
        }
        //split tokens at i
        let exprTokens = basicTokens.splice(0, i);
        //remove closing parenthesis
        basicTokens.shift();
        //TODO multiple
        basic = new FunctionCall(name, parseValue([exprTokens[0]]));
    }  else if (basicTokens[0].type === Type.ENTITY || basicTokens[0].type === Type.OPERATION) {
        //check for name
        basic = Value.createName(basicTokens[0].text);
        basicTokens.shift();
    }else {
        basicTokens.shift();
    }

    //TODO: check for pairs
    //TODO: add curly brackets to the tokens

    return basic;

}

export default parse;
