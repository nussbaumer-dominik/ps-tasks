#!/usr/bin/env node

//1. read file from disc
import fs from 'fs';
import readline from 'readline';
import lex from './services/lexer.mjs';
import {Parser} from './services/parser.mjs';

const filePath = 'demo-code.txt';

// Create a readable stream from the file
const fileStream = fs.createReadStream(filePath);

// Create a readline interface
const rl = readline.createInterface({
    input: fileStream,
    crlfDelay: Infinity // Recognize all instances of CR LF ('\r\n') as a single line break
});

// Event handler for each line
rl.on('line', (line) => {
    console.log(`Line from file: ${line}`);
    let lexResult = lex(line);
    console.log(`Lexing result: \n ${JSON.stringify(lexResult, null, 2)}`);
    let parser = new Parser(lexResult);
    let parseResult = parser.parse();
    console.log(`Parsing result: \n ${JSON.stringify(parseResult, null, 2)}`);
});

// Event handler for the end of the file
rl.on('close', () => {
    console.log('File reading finished.');
});

