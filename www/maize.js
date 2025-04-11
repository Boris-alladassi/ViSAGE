function drawBackground(ctx, width, height) { 
    let gradient = ctx.createLinearGradient(0, 0, 0, height);
    gradient.addColorStop(0, '#def3f7');  
    gradient.addColorStop(0.89, '#eef6eb'); 
    gradient.addColorStop(0.92, '#c2a381');

    ctx.fillStyle = gradient;
    ctx.fillRect(0, 0, width, height);
}

// let yglobal = canvas.height;
function drawMaizePlant(ctx, xglobal, yglobal, height, tasselLength, girth) {
  // const canvas = document.getElementById('myCanvas');
  // const ctx = canvas.getContext('2d');  // Get ctx inside the function
    function drawStalk(ctx, x, y, height, girth) {
      let gr = girth / 3; 
      ctx.fillStyle = '#30a630';
      ctx.strokeStyle = '#30a630';
      ctx.lineWidth = 0.5;
      ctx.beginPath();
      ctx.moveTo(x / 2 - 3 * gr / 2, 0.92 * y);
      ctx.lineTo(x / 2 + 3 * gr / 2, 0.92 * y);
      ctx.lineTo(x / 2 + gr, y - height);
      ctx.lineTo(x / 2 - gr, y - height);
      ctx.closePath();
      ctx.fill();
      ctx.stroke();
    }
  
    function drawArchedCornLeafRight(ctx, xlf, ylf) {
      let x = xlf / 2;
      let y = ylf;
      const gradient = ctx.createLinearGradient(x, y - 100, x, y);
      gradient.addColorStop(0, '#309040');
      gradient.addColorStop(1, 'green');
  
      ctx.fillStyle = gradient;
      ctx.strokeStyle = 'darkgreen'; 
      ctx.lineWidth = 1;
      
      ctx.beginPath();
      ctx.moveTo(x, ylf);
      ctx.bezierCurveTo(x + 0, y - 10, x + 30, y - 35, x + 80, y - 35);
      ctx.quadraticCurveTo(x + 100, y - 35, x + 120, y - 25);
      ctx.bezierCurveTo(x + 65, y - 65, x + 45, y - 55, x + 10, y);
      ctx.closePath();
      ctx.fill();
      ctx.stroke();
  
      ctx.strokeStyle = "lightyellow";
      ctx.lineWidth = 0.5;
      ctx.beginPath();
      ctx.moveTo(x,y);
      ctx.bezierCurveTo(x,y+3, x+50, y-68, x+110, y-30);
      ctx.stroke();
      ctx.closePath();
    }
  
    function drawArchedCornLeafLeft(ctx, xlf, ylf) {
      let x = xlf / 2;
      let y = ylf;
      const gradient = ctx.createLinearGradient(x - 100, y - 100, x, y);
      gradient.addColorStop(0, '#309040');
      gradient.addColorStop(1, 'green');
      ctx.fillStyle = gradient;
      ctx.strokeStyle = 'darkgreen';
      ctx.lineWidth = 1;
  
      ctx.beginPath();
      ctx.moveTo(x, y);
      ctx.bezierCurveTo(x + 0, y - 10, x - 30, y - 35, x - 80, y - 35);
      ctx.quadraticCurveTo(x - 100, y - 35, x - 120, y - 25);
      ctx.bezierCurveTo(x - 65, y - 65, x - 45, y - 55, x - 10, y);
      ctx.closePath();
      ctx.fill();
      ctx.stroke();
      
      ctx.strokeStyle = "lightyellow";
      ctx.lineWidth = 0.5;
      ctx.beginPath();
      ctx.moveTo(x,y);
      ctx.bezierCurveTo(x,y+3, x-50, y-68, x-110, y-30);
      ctx.stroke();
      ctx.closePath();
  
    }
  
    function drawCorn(ctx, xcorn, ycorn, girth, height, tasselLength) {
      ctx.fillStyle = 'green';
      ctx.strokeStyle = 'darkgreen';
      ctx.lineWidth = 1;
  
      let x = xcorn;
      let y = ycorn - 0.66*height;
      let index = 2*girth-0.5*tasselLength-0.02*height;
  
      ctx.beginPath();
      ctx.moveTo(x/2+girth+25+0.6*index, y);
      ctx.ellipse(x/2 +girth+10+0.3*index, y, 15+0.3*index, 50+index, .2, 0, Math.PI * 2, false);
      ctx.closePath();
      ctx.fill();
      ctx.stroke();
    }
  
    function drawTassels(ctx, xtl, ytl, height, tasselLength) {
      let x = xtl/2;
      let y = ytl - height;
      let l ;
      if (tasselLength < 30) {
        l = 30 + tasselLength;
      } else {
        l = 60 + (tasselLength - 30)/2;
      }
  
      ctx.strokeStyle = 'goldenrod';
      ctx.lineWidth = 3.5;
      ctx.beginPath();
      // Lower branch
      ctx.moveTo(x+1, y);
      ctx.bezierCurveTo(x+1, y-l/6, x+10, y-l/5,x+25, y-l/3);
      ctx.moveTo(x-1, y);
      ctx.bezierCurveTo(x-1, y-l/6, x-10, y-l/5,x-25, y-l/3);
      // Middle branch
      ctx.moveTo(x+1.5, y);
      ctx.bezierCurveTo(x+1.5, y-l/3, x+10, y-l/2,x+20, y-3*l/4);
      ctx.moveTo(x-1.5, y);
      ctx.bezierCurveTo(x-1.5, y-l/3, x-10, y-l/2,x-20, y-3*l/4);
      // Upper branch
      ctx.moveTo(x, y);
      ctx.bezierCurveTo(x, y-l/3, x+2, y-5*l/6,x+5, y-l);
      ctx.moveTo(x, y);
      ctx.closePath();
      ctx.stroke();
    }
  
    // This function draws the roots of the maize plant
    function drawRoots(ctx, xrt, yrt, girth) {
      let x = xrt/2;
      let y = 0.92*yrt;
      let rootlength = 10 + girth/4;
      ctx.strokeStyle = 'brown';
      ctx.lineWidth = 1+girth/8;
      ctx.beginPath();
      ctx.moveTo(x, y);
      ctx.bezierCurveTo(x, y, x+2*rootlength, y, x+3*rootlength, y+5);
      ctx.bezierCurveTo(x+3.5*rootlength, y+10, x+5*rootlength, y+8, x+6*rootlength, y+6);
      ctx.moveTo(x, y);
      ctx.bezierCurveTo(x, y, x-2*rootlength, y, x-3*rootlength, y+5);
      ctx.bezierCurveTo(x-3.5*rootlength, y+10, x-5*rootlength, y+8, x-6*rootlength, y+6);
      ctx.moveTo(x, y);
      ctx.bezierCurveTo(x, y, x+10, y, x+1.5*rootlength, y+10);
      ctx.bezierCurveTo(x+20, y+20, x+25, y+25, x+4*rootlength, y+25);
      ctx.moveTo(x, y);
      ctx.bezierCurveTo(x, y, x-10, y, x-1.5*rootlength, y+10);
      ctx.bezierCurveTo(x-20, y+20, x-25, y+25, x-4*rootlength, y+25);
      ctx.moveTo(x, y);
      ctx.bezierCurveTo(x, y, x+5, y, x+rootlength, y+10);
      ctx.moveTo(x, y);
      ctx.bezierCurveTo(x, y, x-5, y, x-rootlength, y+10);
      ctx.moveTo(x, y);
      ctx.bezierCurveTo(x, y, x+10, y, x+3*rootlength, y+15);
      ctx.moveTo(x, y);
      ctx.bezierCurveTo(x, y, x-10, y, x-4*rootlength, y+15);
      ctx.moveTo(x, y);
      ctx.bezierCurveTo(x, y+5, x+5, y+15, x+5, y+35);
      ctx.moveTo(x, y);
      ctx.closePath();
      ctx.stroke();
      
    }
  
    drawTassels(ctx, xglobal, yglobal, height, tasselLength);
    drawCorn(ctx, xglobal, yglobal, girth, height, tasselLength);
  
    let numleaf = Math.floor((height - 100) / 30);
    let ind = 0;
    let topleaf = yglobal - height + 50;
    for (let i = 1; i <= numleaf; i += 2) {
      drawArchedCornLeafRight(ctx, xglobal, topleaf + ind);
      drawArchedCornLeafLeft(ctx, xglobal, topleaf + 30 + ind);
      ind += 60;
    }
  
    drawStalk(ctx, xglobal, yglobal, height, girth);
    drawRoots(ctx, x = xglobal, y = yglobal, girth);
  }

function drawMultipleMaizePlants(ctx, x, y, heightArray, tasselLengthArray, girthArray) {
  for(let i = 0; i < 5; i++) {
    let generation = i * 5;
    // Write the generation number
    ctx.font = '30px Arial';
    ctx.fillStyle = 'black';
    ctx.fillText(`Generation ${generation}`, x/2 - 80, 50);
    // Draw the maize plant
    drawMaizePlant(ctx, xglobal = x, yglobal = y,
      height = heightArray[i], tasselLength = tasselLengthArray[i] , girth = girthArray[i]);
    x += 600;
  }
}
