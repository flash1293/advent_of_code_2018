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

const areaSizes = coords.map(([centerX, centerY], idx) => {
    const otherCenters = [...coords];
    otherCenters.splice(idx, 1);

    const visited = new Set();
    let cellCounter = 0;
    const walkStack = [];

    function walk(x, y) {
        if(visited.has(coordKey(x, y))) return;
        visited.add(coordKey(x, y));
        const homeDistance = manhattan(x, y, centerX, centerY);
        const distances = otherCenters.map(([ox, oy]) => manhattan(ox, oy, x, y));
        const homeIsNearest = distances.reduce((r, d) => r && d > homeDistance, true);
        if(homeIsNearest) {
            if(x === minX || x === maxX || y === minY || y === maxY) {
                cellCounter = Infinity;
                return;
            }
            cellCounter++;
            walkStack.push([x - 1, y]);
            walkStack.push([x + 1, y]);
            walkStack.push([x, y - 1]);
            walkStack.push([x, y + 1]);
        }
    }
    walkStack.push([centerX, centerY]);
    while(walkStack.length > 0 && cellCounter !== Infinity) {
        const [x, y] = walkStack.pop();
        walk(x, y);
    }
    return cellCounter;
});

console.log(areaSizes);
console.log(Math.max.apply(undefined, areaSizes.filter(s => s !== Infinity)));