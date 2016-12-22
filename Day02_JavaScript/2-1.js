var fs = require('fs');

var keypadWidth = 3,
    keypadHeight = 3,
    keypadSize = keypadWidth * keypadHeight;

function moveUp(n) {
    var potentialMove = n - keypadWidth;
    return potentialMove > 0 ? potentialMove : n;
}

function moveDown(n) {
    var potentialMove = n + keypadWidth;
    return potentialMove <= keypadSize ? potentialMove : n;
}

function moveRight(n) {
    var potentialMove = n + 1;
    return potentialMove <= keypadSize && n % 3 !== 0 ? potentialMove : n;
}

function moveLeft(n) {
    var potentialMove = n - 1;
    return potentialMove > 0 && n % 3 !== 1 ? potentialMove : n;
}

function getNumber(n, commands) {
    var num = n;
    for (var i = 0, len = commands.length; i < len; i++) {
        var command = commands.charAt(i);
        if (command == 'U') num = moveUp(num);
        else if (command == 'D') num = moveDown(num);
        else if (command == 'R') num = moveRight(num);
        else if (command == 'L') num = moveLeft(num);
    }
    return num;
}

function readInstructions(instructions) {
    var code = "";
    var num = 5;
    for (var i = 0, len = instructions.length; i < len; i++) {
        num = getNumber(num, instructions[i]);
        code += parseInt(num);
    }
    return code;
}

function solveCodeFromInput(input) {
    var lines = input.match(/[^\r\n]+/g);
    return readInstructions(lines);
}

fs.readFile('input.txt', 'utf8', function(err, input) {
    if (err)
        console.log('Could not read input file.');
    else
        console.log(solveCodeFromInput(input));
});
