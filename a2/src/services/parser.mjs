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

function parseBasic(basicTokens) {
    //check for lambda
    if (basicTokens[0].type === Type.ENTITY && basicTokens.length > 1 && basicTokens[1].type === Type.ASSIGN) {
        let name = basicTokens[0].text;
        basicTokens.shift();
        basicTokens.shift();
        //TODO get scoped expression
        const expression = [basicTokens[0]];
        basicTokens.shift();
        return Basic.createAssignment(new Assign(name, parseBasic(expression)));
    }

    //else check for apply
    return Basic.createValue(parseValue(basicTokens));
}

function parseValue(valueTokens) {
    let basic = null;
    //check for number
    if (valueTokens[0].type === Type.NUMBER) {
        basic = Value.createNumber(valueTokens[0].text);
        valueTokens.shift();
    }
    else if (valueTokens[0].type === Type.ENTITY  && valueTokens[1]?.type === Type.LCURLY) {
        const name = valueTokens[0].text;
        //check for expr
        valueTokens.shift();
        valueTokens.shift();
        //check for closing parenthesis
        let i = valueTokens.length - 1;
        while (valueTokens[i].type !== Type.RCURLY) {
            i--;
        }
        //split tokens at i
        let exprTokens = valueTokens.splice(0, i);
        //remove closing parenthesis
        valueTokens.shift();
        //TODO multiple
        basic = new FunctionCall(name, parseValue([exprTokens[0]]));
    }  else if (valueTokens[0].type === Type.ENTITY || valueTokens[0].type === Type.OPERATION) {
        //check for name
        basic = Value.createName(valueTokens[0].text);
        valueTokens.shift();
    }else {
        valueTokens.shift();
    }

    //TODO: check for pairs
    //TODO: add curly brackets to the tokens

    return basic;

}

export default parse;
