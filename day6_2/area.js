const fs = require("fs");

const data = fs.readFileSync(process.argv[2], { encoding: 'utf8' });

const coords = data.split("\n").map(line => line.replace(",", "").split(" ").map(coord => parseInt(coord)));
const yCoords = coords.map(([_, y]) => y);
const xCoords = coords.map(([x]) => x);
const minY = Math.min.apply(undefined, yCoords);
const maxY = Math.max.apply(undefined, yCoords);
const minX = Math.min.apply(undefined, xCoords);
const maxX = Math.max.apply(undefined, xCoords);

function manhattan(x1, y1, x2, y2) {
    return Math.abs(x1 - x2) + Math.abs(y1 - y2);
}

function coordKey(x, y) {
    return `${x},${y}`;
}

const getCommonAreaSize = (centerX, centerY) => {
    const otherCenters = [...coords];

    const visited = new Set();
    let cellCounter = 0;
    const walkStack = [];

    function walk(x, y) {
        if(visited.has(coordKey(x, y))) return;
        visited.add(coordKey(x, y));
        const allDistances = otherCenters.reduce((s, [ox, oy]) => s + manhattan(ox, oy, x, y), 0);
        if(allDistances < 10000) {
            cellCounter++;
            walkStack.push([x - 1, y]);
            walkStack.push([x + 1, y]);
            walkStack.push([x, y - 1]);
            walkStack.push([x, y + 1]);
        }
    }
    walkStack.push([centerX, centerY]);
    while(walkStack.length > 0) {
        const [x, y] = walkStack.pop();
        walk(x, y);
    }
    return cellCounter;
};

console.log(getCommonAreaSize(minX + Math.floor((maxX - minX) / 2), minY + Math.floor((maxY - minY) / 2)));