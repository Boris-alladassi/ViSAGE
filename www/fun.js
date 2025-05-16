
function drawBackground(ctx, width, height) {
    let gradient = ctx.createLinearGradient(0, 0, 0, height);
    gradient.addColorStop(0, '#def3f7');  // Light blue top
    gradient.addColorStop(0.89, '#eef6eb'); // Keep most of the canvas green
    gradient.addColorStop(0.92, '#c2a381');  // Light brown bottom 12%
    
    ctx.fillStyle = gradient;
    ctx.fillRect(0, 0, width, height);
}

// Corn //////////////////////////////////////////////
function drawFunCorn(ctx, xglobal, yglobal, girth, height, tasselLength, faceType) {
  // let ylowerglobal = 500;
  // let heightmini = 335;
  if(height < 310){height = 310}
  // This function draws the cob of the ear.
  function drawCob(ctx, xcob, ycob, height) {
    let x = xcob/2;
    let y = ycob-height;
    let ylower = ycob-height+235;
    // let ylower = ylowerglobal;
    // if(height >= heightmini){ylower = ycob-height+235;}
    // ctx.fillStyle = 'goldenrod';
    ctx.strokeStyle = '#def3f7';
    ctx.lineWidth = 12;
    let soff = 30;
    let boff = 60;

    ctx.beginPath();
    ctx.moveTo(x - soff, y + 15);
    ctx.quadraticCurveTo(x, y - 15, x + soff, y + 15); //upper concave
    ctx.quadraticCurveTo(x + boff, ylower-50, x + soff, ylower - 30); // right arc

    // ctx.moveTo(x + 3 * girth, 0.75 * y - 20);
    ctx.quadraticCurveTo(x, ylower, x - soff, ylower - 30);
    ctx.quadraticCurveTo(x - boff, ylower-50, x - soff, y + 15);
    ctx.closePath();
    ctx.stroke();
    // ctx.fill();
  }

  function drawHusk(xhusk, yhusk , height) {
    x = xhusk/2;
    let y = yhusk-height+235;
    // let y = ylowerglobal;
    // if(height >=heightmini){y = yhusk-height+235;}
    ctx.fillStyle = 'green';
    ctx.strokeStyle = 'darkgreen';
    ctx.lineWidth = 1;
    // Right husk
    ctx.beginPath();
    ctx.moveTo(x, y);
    ctx.bezierCurveTo(x, y-100, x+60, y-95, x+80, y-90);
    ctx.bezierCurveTo(x+65, y-101, x+65, y-98, x+55, y-75);
    ctx.bezierCurveTo(x+55, y-50, x+48, y-10, x, y);
    ctx.fill();
    ctx.closePath();
    ctx.moveTo(x, y);
    ctx.bezierCurveTo(x+35, y-70, x+45, y-75, x+58, y-90);
    ctx.stroke();
    // Left husk
    ctx.beginPath();
    ctx.moveTo(x+10, y);
    ctx.bezierCurveTo(x, y-100, x-60, y-95, x-80, y-90);
    ctx.bezierCurveTo(x-65, y-101, x-65, y-98, x-55, y-75);
    ctx.bezierCurveTo(x-55, y-50, x-48, y-10, x, y);
    ctx.fill();
    ctx.closePath();
    ctx.moveTo(x, y);
    ctx.bezierCurveTo(x-35, y-70, x-45, y-75, x-58, y-90);
    ctx.stroke();    
  }

  // This function draws the hands of the maize ear
  function drawHands(ctx, xhand, yhand, height) {
    let x = xhand/2;
    let y = yhand - height + 120;
    // let y = ylowerglobal-115;
    // if(height>=heightmini){y = yhand - height + 120;}
    
    // Create a variable to control the inclination of the hands
    let deltaY = 0;
    let deltaY1 = 0;
    let deltaX = 0;
    let yfingers = 0;

    if(height < 400){
      deltaY = Math.pow((600-height)/100, 3); // Controls the inclination
      deltaX = Math.sqrt(900 - 3*deltaY);
      yfingers = 0;

      // Drawing the fingers
      ctx.fillStyle = "brown";
      ctx.strokeStyle = "brown";
      ctx.lineWidth = 5;
      // Left fingers
      ctx.beginPath();
      ctx.moveTo(x-30-deltaX, y+deltaY-2);
      ctx.lineTo(x-47-deltaX, y+deltaY+10);
      ctx.moveTo(x-30-deltaX, y+deltaY);
      ctx.lineTo(x-42-deltaX, y+deltaY+18);
      ctx.moveTo(x-28-deltaX, y+deltaY);
      ctx.lineTo(x-34-deltaX, y+deltaY+22);
      ctx.closePath();
      ctx.stroke();
    // Right fingers
      ctx.beginPath();
      ctx.moveTo(x+30+deltaX, y+deltaY-2);
      ctx.lineTo(x+47+deltaX, y+deltaY+10);
      ctx.moveTo(x+30+deltaX, y+deltaY);
      ctx.lineTo(x+42+deltaX, y+deltaY+18);
      ctx.moveTo(x+28+deltaX, y+deltaY);
      ctx.lineTo(x+34+deltaX, y+deltaY+22);
      ctx.closePath();
      ctx.stroke();
    } else if(height > 400) {
        deltaY1 = -Math.pow(height/100, 3);
        deltaX = Math.sqrt(900 + 3*deltaY1);
        deltaY = 0.7*deltaY1;
        yfingers = 0;

        // Drawing the fingers
      ctx.fillStyle = "brown";
      ctx.strokeStyle = "brown";
      ctx.lineWidth = 5;
        // Left fingers
      ctx.beginPath();
      ctx.moveTo(x-32-deltaX, y+deltaY);
      ctx.lineTo(x-47-deltaX, y+deltaY-10);
      ctx.moveTo(x-30-deltaX, y+deltaY);
      ctx.lineTo(x-42-deltaX, y+deltaY-18);
      ctx.moveTo(x-28-deltaX, y+deltaY);
      ctx.lineTo(x-35-deltaX, y+deltaY-25);
      ctx.closePath();
      ctx.stroke();
    // Right fingers
      ctx.beginPath();
      ctx.moveTo(x+32+deltaX, y+deltaY);
      ctx.lineTo(x+47+deltaX, y+deltaY-10);
      ctx.moveTo(x+30+deltaX, y+deltaY);
      ctx.lineTo(x+42+deltaX, y+deltaY-18);
      ctx.moveTo(x+28+deltaX, y+deltaY);
      ctx.lineTo(x+35+deltaX, y+deltaY-25);
      ctx.closePath();
      ctx.stroke();
    } else { 
        deltaY = 0;
        deltaX = Math.sqrt(900 - deltaY/5);
        yfingers = 0;
        // Drawing the fingers
        ctx.fillStyle = "brown";
        ctx.strokeStyle = "brown";
        ctx.lineWidth = 5;
        // Left fingers
        ctx.beginPath();
        ctx.moveTo(x-30-deltaX, y+deltaY-2);
        ctx.lineTo(x-47-deltaX, y+deltaY-10);
        ctx.moveTo(x-30-deltaX, y+deltaY);
        ctx.lineTo(x-47-deltaX, y+deltaY);
        ctx.moveTo(x-30-deltaX, y+deltaY+2);
        ctx.lineTo(x-47-deltaX, y+deltaY+10);
        ctx.closePath();
        ctx.stroke();
        // Right fingers
        ctx.beginPath();
        ctx.moveTo(x+30+deltaX, y+deltaY-2);
        ctx.lineTo(x+47+deltaX, y+deltaY-10);
        ctx.moveTo(x+30+deltaX, y+deltaY);
        ctx.lineTo(x+47+deltaX, y+deltaY);
        ctx.moveTo(x+30+deltaX, y+deltaY+2);
        ctx.lineTo(x+47+deltaX, y+deltaY+10);
        ctx.closePath();
        ctx.stroke();
    }

    // Straight left arm
    ctx.strokeStyle = "darkgreen";
    ctx.lineWidth = 10;
    ctx.beginPath();
    ctx.moveTo(x-30, y);
    ctx.lineTo(x-30-deltaX, y+deltaY);
    ctx.stroke();
    ctx.closePath();

    // Straight right arm
    ctx.beginPath();
    ctx.moveTo(x+30, y);
    ctx.lineTo(x+30+deltaX, y+deltaY);
    ctx.stroke();
    ctx.closePath();
    
  }

  function drawSadHands(ctx, xhand, yhand, height) {
    x = xhand/2;
    y = yhand - height + 120;
    ctx.strokeStyle = "darkgreen";
    ctx.lineWidth = 10;

    // Curved left arm
    ctx.beginPath();
    ctx.moveTo(x-30, y-20);
    ctx.quadraticCurveTo(x-30-60, y-20, x-30+10, y+20);
    ctx.stroke();
    ctx.closePath();

    // Curved right arm
    ctx.beginPath();
    ctx.moveTo(x+30, y-20);
    ctx.quadraticCurveTo(x+30+55, y-20, x+30-10, y+20);
    ctx.stroke();
    ctx.closePath();


    ctx.fillStyle = "brown";
    ctx.strokeStyle = "brown";
    ctx.lineWidth = 5;
    // Right fingers 
    ctx.beginPath();
    ctx.moveTo(x+30-10, y+20);
    ctx.lineTo(x+30-25, y+30);
    ctx.moveTo(x+30-10, y+20);
    ctx.lineTo(x+30-30, y+20);
    ctx.moveTo(x+30-10, y+20);
    ctx.lineTo(x+30-25, y+10);
    ctx.closePath();
    ctx.stroke();

    //Left fingers
    ctx.beginPath();
    ctx.moveTo(x-30+10, y+20);
    ctx.lineTo(x-30+20, y+30);
    ctx.moveTo(x-30+10, y+20);
    ctx.lineTo(x-30+25, y+20);
    ctx.moveTo(x-30+10, y+20);
    ctx.lineTo(x-30+25, y+10);
    ctx.closePath();
    ctx.stroke();
  }

  // This function draws the stalk of the maize plant
  function drawStalk(ctx, xstk, ystk, height, girth) {
    let x = xstk/2;
    let y = ystk;
    let gr = girth; 
    let topstalk = y-height+219
    ctx.fillStyle = '#90EE90';
    ctx.strokeStyle = 'darkgreen';
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.moveTo(x-gr/2, 0.92*y);
    ctx.quadraticCurveTo(x, 0.92*y +5, x+gr/2, 0.92*y);
    ctx.lineTo(x + gr/2  , topstalk);
    ctx.quadraticCurveTo(x, topstalk - 5, x - gr/2, topstalk);
    ctx.closePath();
    ctx.stroke();
    ctx.fill();
    
    ctx.strokeStyle = 'darkgreen';
    ctx.lineWidth = 0.4;
    ctx.beginPath();
    ctx.moveTo(x+2, 0.92*y);
    ctx.lineTo(x+2, topstalk+100);
    ctx.moveTo(x-2, topstalk+50);
    ctx.lineTo(x-2, 0.85*y);
    ctx.stroke();
    ctx.closePath();

    ctx.lineWidth = 0.4;
    ctx.fillStyle = '#4CBB17';
    ctx.beginPath();
    ctx.moveTo(x+gr/2, 0.82*y);
    ctx.ellipse(x, topstalk+130, gr/2, 2, 0, 0, Math.PI*2, false);
    ctx.stroke();
    ctx.closePath();
    ctx.fill();
  }

  // This function draws the roots of the maize plant
  function drawRoots(ctx, xrt, yrt) {
    let x = xrt/2;
    let y = 0.92*yrt;
    ctx.strokeStyle = 'brown';
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.moveTo(x, y);
    ctx.bezierCurveTo(x, y, x+20, y, x+30, y+5);
    ctx.bezierCurveTo(x+35, y+10, x+50, y+8, x+60, y+6);
    ctx.moveTo(x, y);
    ctx.bezierCurveTo(x, y, x-20, y, x-30, y+5);
    ctx.bezierCurveTo(x-35, y+10, x-50, y+8, x-60, y+6);
    ctx.moveTo(x, y);
    ctx.bezierCurveTo(x, y, x+10, y, x+15, y+10);
    ctx.bezierCurveTo(x+20, y+20, x+25, y+25, x+40, y+25);
    ctx.moveTo(x, y);
    ctx.bezierCurveTo(x, y, x-10, y, x-15, y+10);
    ctx.bezierCurveTo(x-20, y+20, x-25, y+25, x-40, y+25);
    ctx.moveTo(x, y);
    ctx.bezierCurveTo(x, y, x+5, y, x+10, y+10);
    ctx.moveTo(x, y);
    ctx.bezierCurveTo(x, y, x-5, y, x-10, y+10);
    ctx.moveTo(x, y);
    ctx.bezierCurveTo(x, y, x+10, y, x+30, y+15);
    ctx.moveTo(x, y);
    ctx.bezierCurveTo(x, y, x-10, y, x-40, y+15);
    ctx.moveTo(x, y);
    ctx.bezierCurveTo(x, y+5, x+5, y+15, x+5, y+35);
    ctx.moveTo(x, y);
    ctx.closePath();
    ctx.stroke();
    
  }

  // ctx.arc(x, y, radius, startAngle, endAngle, counterclockwise);

  // This function draws the face of the maize plant
  function drawHappyFace(ctx, xface, yface, height, tasselLength, girth) {
    x = xface/2;
    y = yface;
    let yeyes = y-height+80;
    // let yeyes = ylowerglobal-155;
    // if(height>=heightmini){yeyes = y-height+80;}

    // Eyes white
    ctx.fillStyle = "white";
    ctx.strokeStyle = "black";
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.arc(x-1.3*girth, yeyes, girth, 0, Math.PI * 2);
    ctx.fill();
    ctx.stroke();
    ctx.beginPath();
    ctx.arc(x+1.3*girth, yeyes, girth, 0, Math.PI * 2);
    ctx.fill();
    ctx.stroke();
  
    // Eyes black
    ctx.fillStyle = "black";
    ctx.beginPath();
    ctx.arc(x-1.3*girth, yeyes, 0.5*girth, 0, Math.PI * 2);
    ctx.fill();
    ctx.beginPath();
    ctx.arc(x+1.3*girth, yeyes, 0.5*girth, 0, Math.PI * 2);
    ctx.fill();
    ctx.closePath();

    // Happy mouth (upward arc)
    let ymouth = y-height+100;
    // let ymouth = ylowerglobal-135;
    // if(height>= heightmini){ymouth = y-height+100;}
    ctx.beginPath();
    ctx.strokeStyle = "black";
    ctx.fillStyle = "darkred";
    ctx.lineWidth = 2;
    // ctx.arc(x, ymouth, tasselLength/3, 0,  Math.PI);
    ctx.moveTo(x-10-tasselLength/4, ymouth);
    ctx.quadraticCurveTo(x, ymouth+30+tasselLength/8, x+10+tasselLength/4, ymouth);
    ctx.moveTo(x+10+tasselLength/4, ymouth);
    ctx.quadraticCurveTo(x, ymouth+10, x-10-tasselLength/4, ymouth);
    ctx.stroke();
    ctx.fill();
    ctx.closePath();
  }

  // This function draws the face of the maize plant
  function drawSadFace(ctx, xface, yface, height) {
    x = xface/2;
    y = yface;
    // Eyes
    ctx.fillStyle = "white";
    ctx.strokeStyle = "black";
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.arc(x-1.2*10, y-0.8*height, 10, 0, Math.PI * 2);
    ctx.fill();
    ctx.stroke();
    ctx.beginPath();
    ctx.arc(x+1.2*10, y-0.8*height, 10, 0, Math.PI * 2);
    ctx.fill();
    ctx.stroke();
  
    // Eyes
    ctx.fillStyle = "black";
    ctx.beginPath();
    ctx.arc(x-1.2*10+3, y-0.8*height+3, 5, 0, Math.PI * 2);
    ctx.fill();
    ctx.beginPath();
    ctx.arc(x+1.2*10-3, y-0.8*height+3, 5, 0, Math.PI * 2);
    ctx.fill();
    ctx.closePath();

    // sad mouth (upward arc)
    ctx.beginPath();
    ctx.strokeStyle = "black";
    ctx.fillStyle = "darkred";
    ctx.lineWidth = 5;
    ctx.arc(x, y-0.70*height, 15, Math.PI+0.3,  -0.3, false);
    ctx.moveTo(x-10, y-0.75*height);
    // ctx.quadraticCurveTo(x, y-0.65*height, x+10, y-0.75*height);
    ctx.stroke();
    // ctx.fill();
    ctx.closePath();
  }
// This function draws the face of the maize plant
function drawSillyFace(ctx, xface, yface, height) {
  x = xface/2;
  y = yface;
  // Eyes
  ctx.fillStyle = "white";
  ctx.strokeStyle = "black";
  ctx.lineWidth = 1;
  ctx.beginPath();
  ctx.arc(x-1.2*10, y-0.8*height, 10, 0, Math.PI * 2);
  ctx.fill();
  ctx.stroke();
  ctx.beginPath();
  ctx.arc(x+1.2*10, y-0.8*height, 8, 0, Math.PI * 2);
  ctx.fill();
  ctx.stroke();

  // Eyes
  ctx.fillStyle = "black";
  ctx.beginPath();
  ctx.arc(x-1.2*10+3, y-0.8*height-5, 5, 0, Math.PI * 2);
  ctx.fill();
  ctx.beginPath();
  ctx.arc(x+1.2*10, y-0.8*height+2, 5, 0, Math.PI * 2);
  ctx.fill();
  ctx.closePath();

  // silly mouth (upward arc)
  ctx.beginPath();
  ctx.strokeStyle = "black";
  ctx.fillStyle = "white";
  ctx.lineWidth = 2;
  ctx.moveTo(x-20, y-0.75*height);
  ctx.arc(x, y-0.75*height, 20, 0,  Math.PI);
  ctx.fill();
  ctx.stroke();  
  ctx.closePath();

  ctx.beginPath();
  ctx.strokeStyle = "black";
  ctx.fillStyle = "darkred";
  ctx.moveTo(x-5, y-0.75*height);
  // ctx.arc(x, y-0.75*height, 5, 0,  Math.PI);
  ctx.quadraticCurveTo(x-10, y-0.6*height, x+15, y-0.75*height);
  ctx.fill();
  ctx.stroke();  
  ctx.closePath();
}

  // This function draws the tassels of the maize plant
  function drawTassels(ctx, xtl, ytl, height, tasselLength) {
    let x = xtl/2;
    let y = ytl - height+9;
    let l ;
    if (tasselLength < 30) {
      l = 30 + tasselLength;
    } else {
      l = 60 + (tasselLength - 30)/2;
    }

    ctx.strokeStyle = 'goldenrod';
    ctx.lineWidth = 4;
    ctx.beginPath();
    // Lower branch
    ctx.moveTo(x+1, y);
    ctx.bezierCurveTo(x+1, y-l/6, x+10, y-l/2,x+40, y-l/5);
    ctx.moveTo(x-1, y);
    ctx.bezierCurveTo(x-1, y-l/6, x-10, y-l/2,x-40, y-l/5);
    // Middle branch
    ctx.moveTo(x+1.5, y);
    ctx.bezierCurveTo(x+1.5, y-l/3, x+10, y-l/2,x+30, y-3*l/4);
    ctx.moveTo(x-1.5, y);
    ctx.bezierCurveTo(x-1.5, y-l/3, x-10, y-l/2,x-20, y-3*l/4);
    // Upper branch
    ctx.moveTo(x, y);
    ctx.bezierCurveTo(x, y-l/3, x+2, y-5*l/6,x+10, y-l);
    ctx.arc(x+10, y-l, 5, 0, Math.PI*2);
    ctx.moveTo(x, y);
    ctx.closePath();
    ctx.stroke();
  }

  function drawKernels(ctx, xkrl, ykrl, girth, height) {
    let x = xkrl/2;
    let y = ykrl-height+17;
    ctx.fillStyle = 'gold';
    ctx.strokeStyle = 'goldenrod';
    ctx.lineWidth = 2;
    for (let row = 0; row < 8; row++) {
        for (let col = -2; col <= 2; col++) {
            let xOffset = col * 10 + (row % 2) * 5; // Staggered rows
            let yOffset = row * 10;
            ctx.beginPath();
            ctx.arc(x -3+ xOffset, y + yOffset, 8, 0, Math.PI * 2);
            ctx.fill();
            ctx.stroke();
        }
    }

    for (let row = 0; row < 15; row++) {
      for (let col = -3; col <= 3; col++) {
          let xOffset = col * 10 + (row % 2) * 5; // Staggered rows
          let yOffset = row * 10;
          ctx.beginPath();
          ctx.arc(x -4 + xOffset, y +50 + yOffset, 7, 0, Math.PI * 2);
          ctx.fill();
          ctx.stroke();
      }
    }
  }

  
  drawKernels(ctx, xkernel = xglobal, ykernel = yglobal, girth, height);
  drawCob(ctx, xcob = xglobal, ycob = yglobal, height);
  drawHusk(xhusk = xglobal, yhusk = yglobal, height, girth);
  if(faceType == 'happy') {
    drawHappyFace(ctx, xface = xglobal, yface = yglobal, height, tasselLength, girth);
    drawHands(ctx, xhand = xglobal, yhand = yglobal, height);
  } else if(faceType == 'silly') {
    drawSillyFace(ctx, xface = xglobal, yface = yglobal, height);
    drawHands(ctx, xhand = xglobal, yhand = yglobal, height);
  } else {
    drawSadFace(ctx, xface = xglobal, yface = yglobal, height);
    drawSadHands(ctx, xhand = xglobal, yhand = yglobal, height);
  }
  drawStalk(ctx, xstk = xglobal, ystk = yglobal, height, girth);
  
  drawTassels(ctx, xtl = xglobal, ytl = yglobal, height, tasselLength);
  drawRoots(ctx, xrt = xglobal, yrt = yglobal);
}
  
function drawMultipleFunCorn(ctx, x, y, heightArray, tasselLengthArray, girthArray) {
  for(let i = 0; i < 5; i++) {
    let generation = i * 5;
    // Write the generation number
    ctx.font = '30px Arial';
    ctx.fillStyle = 'black';
    ctx.fillText(`Generation ${generation}`, x/2 - 80, 50);
    // Draw the maize plant
    drawFunCorn(ctx, xglobal = x, yglobal = y, girth = girthArray[i],
      height = heightArray[i], tasselLength = tasselLengthArray[i] ,  faceType = "happy");
    x += 580;
  }
}

function drawThreshold(ctx, xthresh, ythresh, margin = 100) {
  let x = xthresh;
  let y = ythresh/3;
  ctx.strokeStyle = 'blue';
  ctx.lineWidth = 2;
  ctx.beginPath();
  ctx.moveTo(0, y);
  ctx.lineTo(x, y);
  ctx.moveTo(x, y+margin);
  ctx.lineTo(0, y+margin);
  ctx.stroke();
  ctx.closePath();
}

// Avocado //////////////////////////////////////////////////////////
function drawSun(ctx, xsun,ysun){
  // This function draws the sun in the top right corner of the canvas
  let x = xsun;
  let y = 0;
  ctx.fillStyle = "gold";
  
  ctx.beginPath();
  ctx.moveTo(xsun, y);
  ctx.arc(x,y, ysun/10, Math.PI/2, Math.PI, false);
  ctx.fill();
  ctx.closePath();
  
  // Solar rays
  ctx.beginPath();
  ctx.strokeStyle = "#FFED8A";
  ctx.lineWidth = 3
  ctx.moveTo(0.95*xsun, 0.02*ysun);
  ctx.lineTo(0.9*xsun, 0.05*ysun);

  ctx.moveTo(0.95*xsun, 0.09*ysun);
  ctx.lineTo(0.89*xsun, 0.2*ysun);

  ctx.moveTo(0.98*xsun, 0.12*ysun);
  ctx.lineTo(0.97*xsun, 0.22*ysun); 
  ctx.stroke();
  ctx.closePath();
}

function drawFunnyAvocado(ctx, xglobal, yglobal, height, width, pitSize) {
  let avocadoHeight = height*0.75;
  let avocadoWidth = width*0.8;
  const centerX = xglobal/2;
  const centerY = yglobal/2;

  // Adjusted width to be slightly wider at the base
  const bodyYradius = avocadoHeight;
  const bodyXradius = avocadoWidth;

  // Avocado Body (Halved) //////////////////////////////////////////////////////////
  // First layer greener
  const gradient = ctx.createLinearGradient(0, centerY - avocadoHeight, 0, centerY + avocadoHeight);
  gradient.addColorStop(0, "#A7D948"); // Light green
  gradient.addColorStop(1, "#6B8E23"); // Dark green

  let bodyY = yglobal - 2*height;
  ctx.fillStyle = gradient;
  ctx.strokeStyle = "darkgreen";
  ctx.lineWidth = 4;
  ctx.beginPath();
  ctx.moveTo(centerX+avocadoWidth, bodyY+avocadoHeight);
  ctx.ellipse(centerX, bodyY+avocadoHeight, bodyXradius, bodyYradius,0, 0*Math.PI, 2*Math.PI,false)
  // ctx.moveTo(centerX, centerY-avocadoHeight);
  // ctx.arc(centerX, centerY-avocadoHeight*0.9, topWidth, 0, 2*Math.PI, false);
  ctx.closePath();
  ctx.fill();
  ctx.stroke();
  
  // Second layer yellowish //////////////////////////////////////////////////////////
  const gradient2 = ctx.createLinearGradient(0, centerY - avocadoHeight, 0, centerY + avocadoHeight);
  gradient2.addColorStop(0, "#bfd80d"); // Light green
  gradient2.addColorStop(1, "#f4f42f"); // Dark green
  ctx.fillStyle = gradient2;
  ctx.beginPath();
  ctx.moveTo(centerX+avocadoWidth, bodyY+avocadoHeight);
  ctx.ellipse(centerX, bodyY+avocadoHeight-5, 0.77*bodyXradius, 0.85*bodyYradius,0, 0, 2*Math.PI,false)
  ctx.closePath();
  ctx.fill();

  // Brown Pit
  ctx.fillStyle = "#8B5A2B";
  ctx.beginPath();
  ctx.moveTo(centerX+pitSize, bodyY+avocadoHeight);
  ctx.ellipse(centerX, bodyY+avocadoHeight-0.35*pitSize, pitSize, 1.5*pitSize, 0, 0, 2*Math.PI,false);
  ctx.fill();
  ctx.closePath();

  // Sunglasses //////////////////////////////////////////////////////////
  let eyescenterx = avocadoWidth/2.2;
  let eyescentery = 0.7*avocadoHeight;
  ctx.fillStyle = "blue"
  ctx.strokeStyle = "blue";
  ctx.lineWidth = 10;
  ctx.beginPath();
  ctx.arc(centerX - eyescenterx, bodyY + eyescentery, avocadoWidth/3, 0, Math.PI * 2);
  ctx.arc(centerX + eyescenterx, bodyY + eyescentery, avocadoWidth/3, 0, Math.PI * 2);
  ctx.stroke();
  ctx.fill();
  ctx.closePath();

  ctx.fillStyle = "black";
  ctx.beginPath();
  ctx.arc(centerX - eyescenterx, bodyY + eyescentery, avocadoWidth/3.5, 0, Math.PI * 2);
  ctx.arc(centerX + eyescenterx, bodyY + eyescentery, avocadoWidth/3.5, 0, Math.PI * 2);
  ctx.fill();
  ctx.closePath();

  ctx.strokeStyle = "blue";
  ctx.lineWidth = 6;
  ctx.beginPath();
  ctx.moveTo(centerX - avocadoWidth/6, bodyY + avocadoHeight/4);
  // ctx.lineTo(centerX + avocadoWidth/6, bodyY + avocadoHeight/4);
  ctx.moveTo(centerX +  2.3*avocadoWidth/3, bodyY + eyescentery);
  ctx.lineTo(centerX + avocadoWidth*0.92, bodyY+eyescentery-avocadoHeight/9);
  ctx.moveTo(centerX - 2.3*avocadoWidth/3, bodyY + eyescentery);
  ctx.lineTo(centerX - avocadoWidth*0.92, bodyY+eyescentery-avocadoHeight/9);
  ctx.stroke();
  ctx.closePath();

  // Smiley Mouth //////////////////////////////////////////////////////////
  ctx.strokeStyle = "black";
  ctx.fillStyle = "darkred";
  ctx.lineWidth = 2;
  ctx.beginPath();
  let mouthY = bodyY+avocadoHeight;
  ctx.moveTo(centerX - avocadoWidth/5, mouthY);
  ctx.quadraticCurveTo(centerX, mouthY+ avocadoHeight/20, centerX + avocadoWidth/5, mouthY);
  ctx.moveTo(centerX + avocadoWidth/5, mouthY);
  ctx.quadraticCurveTo(centerX, mouthY+ avocadoHeight/5, centerX-avocadoWidth/5, mouthY);
  ctx.stroke();
  ctx.fill();
  ctx.closePath();

  // ctx.strokeStyle = "black";
  ctx.fillStyle = "white";
  ctx.beginPath();
  ctx.moveTo(centerX - avocadoWidth/5, mouthY);
  ctx.quadraticCurveTo(centerX, mouthY+ avocadoHeight/20, centerX+avocadoWidth/5, mouthY);
  ctx.moveTo(centerX+avocadoWidth/5, mouthY);
  ctx.quadraticCurveTo(centerX, mouthY + avocadoHeight/10, centerX-avocadoWidth/5, mouthY);
  ctx.closePath();
  ctx.fill();
  
  // Curved Arms //////////////////////////////////////////////////////////
  ctx.strokeStyle = "#6B8E23";
  ctx.lineWidth = avocadoWidth/10;
  yarms = 0.8*avocadoHeight;
  ctx.beginPath();
  ctx.moveTo(centerX - 0.9*avocadoWidth, bodyY+yarms);
  ctx.quadraticCurveTo(centerX - 1.5*avocadoWidth, bodyY + 1.1*yarms, 
      centerX - 1.2*avocadoWidth, bodyY +1.5*yarms);
  ctx.stroke();
  ctx.closePath();

  ctx.beginPath();
  ctx.moveTo(centerX + 0.9*avocadoWidth, bodyY+yarms);
  ctx.quadraticCurveTo(centerX + 1.5*avocadoWidth, bodyY + 1.1*yarms, 
      centerX + 1.2*avocadoWidth, bodyY + 1.5*yarms);
  ctx.stroke();
  ctx.closePath();

  // Curved Legs //////////////////////////////////////////////////////////
  // let legsx = ;
  ctx.strokeStyle = "#6B8E23";
  ctx.lineWidth = avocadoWidth/8;
  let legsy = 1.8*centerY;
  ctx.beginPath();
  ctx.moveTo(centerX - 0.5*avocadoWidth, bodyY + 1.65*avocadoHeight);
  ctx.quadraticCurveTo(centerX - avocadoWidth, bodyY + 1.75*avocadoHeight, 
      centerX - 0.7*avocadoWidth, legsy);
  ctx.stroke();
  ctx.closePath();

  ctx.beginPath();
  ctx.strokeStyle = "#6B8E23";
  ctx.moveTo(centerX + 0.5*avocadoWidth, bodyY + 1.65*avocadoHeight);
  ctx.quadraticCurveTo(centerX + avocadoWidth, bodyY + 1.75*avocadoHeight, 
      centerX + 0.7*avocadoWidth, legsy);
  ctx.stroke();
  ctx.closePath();

  // Blue right shoes //////////////////////////////////////////////////////////
  ctx.beginPath();
  ctx.fillStyle = "white";
  let shoesY = 1.8*centerY;
  let shoesY2 = 1.82*centerY;
  ctx.moveTo(centerX + 0.67*avocadoWidth, shoesY2);
  ctx.ellipse(centerX+0.8*avocadoWidth, shoesY2, avocadoWidth/5, 10, 0, 0, 2*Math.PI);
  ctx.fill();

  ctx.beginPath();
  ctx.fillStyle = "blue";
  ctx.moveTo(centerX + 0.67*avocadoWidth, shoesY);
  ctx.ellipse(centerX+0.8*avocadoWidth, shoesY, avocadoWidth/5, 10, 0, 0, 2*Math.PI);
  ctx.fill();
  ctx.closePath();
  // left shoes
  ctx.beginPath();
  ctx.fillStyle = "white";
  ctx.moveTo(centerX - 0.67*avocadoWidth, shoesY2);
  ctx.ellipse(centerX-0.8*avocadoWidth, shoesY2, avocadoWidth/5, 10, 0, 0, 2*Math.PI);
  ctx.fill();

  ctx.beginPath();
  ctx.fillStyle = "blue";
  ctx.moveTo(centerX - 0.67*avocadoWidth, shoesY);
  ctx.ellipse(centerX-0.8*avocadoWidth, shoesY, avocadoWidth/5, 10, 0, 0, 2*Math.PI);
  ctx.fill();
  ctx.closePath();

  //fingers
  ctx.fillStyle = "black";
  let handX = 1.2*avocadoWidth;
  let handY = bodyY + 1.5*yarms;
  handradius = 0.1*yarms;
  ctx.beginPath();
  // ctx.moveTo(centerX - 1.2*avocadoWidth, centerY + avocadoHeight/6);
  // ctx.moveTo(centerX + 1.2*avocadoWidth, centerY + avocadoHeight/6);
  ctx.ellipse(centerX + handX, handY, handradius, handradius, 0, 0, Math.PI * 2); // Finger 1
  ctx.ellipse(centerX -handX, handY, handradius, handradius, 0, 0, Math.PI * 2); // Finger 2
  ctx.fill();
}

function fruitArea(height, width, pitSize) {
  let xradius = 0.8*width;
  let yradius = 0.75*height;
  let pitradius = pitSize;
  let totalArea = 4*Math.PI * xradius * yradius/3;
  //pit size was 0.5*height and 0.5*width
  let pitArea = Math.PI * (0.1*xradius+pitradius) * (0.1*yradius+pitradius)/3;
  let area_avocado = Math.round((totalArea - pitArea)/100);
  if (area_avocado < 5) {
    return 5;
  } else {
    return area_avocado;
  }
}

function drawMultipleAvocados(ctx, x, y, heightArray, widthArray, pitArray) {
  for(let i = 0; i < 5; i++) {
      let generation = i * 5;
      let fleshArea = fruitArea(heightArray[i], widthArray[i], pitArray[i]);
      // Write the generation number
      ctx.font = '30px Arial';
      ctx.fillStyle = 'black';
      ctx.fillText(`Generation ${generation}`, x/2 - 80, 50);
      ctx.fillText(`Flesh area = ${fleshArea}`, x/2 - 100, 0.98*y);
      // Draw the maize plant
      drawFunnyAvocado(ctx, xglobal = x, yglobal = y, heightArray[i],widthArray[i], pitArray[i]);
      x += 580;
    }
}

// Strawberry //////////////////////////////////////////////////////////
function drawFunnyStrawberry(ctx, xglobal, yglobal, height, width, seedSize) {
    let strawberryHeight = 0.9*height;
    let strawberryWidth = width;
    const centerX = xglobal/2;
    const centerY = yglobal/2;

    // Gradient for the strawberry body
    const gradient = ctx.createLinearGradient(0, centerY - strawberryHeight/2, 0, centerY + strawberryHeight/2);
    gradient.addColorStop(0, "#ff4d4d"); // Light red
    gradient.addColorStop(1, "#cc0000"); // Dark red

    let bodyY = yglobal - height;
    ctx.fillStyle = gradient;
    ctx.strokeStyle = "darkred";
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.moveTo(centerX, bodyY);
    ctx.bezierCurveTo(centerX + strawberryWidth, bodyY + 0.1*strawberryHeight, 
        centerX + 0.3*strawberryWidth, bodyY + 0.6*strawberryHeight, centerX,  bodyY+0.7*strawberryHeight);
    ctx.moveTo(centerX, bodyY);
    ctx.bezierCurveTo(centerX - strawberryWidth, bodyY + 0.1*strawberryHeight, 
        centerX - 0.3*strawberryWidth, bodyY + 0.6*strawberryHeight, centerX, bodyY+0.7*strawberryHeight);
    ctx.moveTo(centerX, centerY - strawberryHeight/2);
    ctx.closePath();
    ctx.fill();
    ctx.stroke();

    // Seeds (yellow dots) //////////////////////////////////////////////////////////
    ctx.fillStyle = "gold";
    let seedRow = centerX -0.38*strawberryWidth;
    let seeColumn = bodyY + 0.15*strawberryHeight;
    let rowSpacing = strawberryWidth/5;
    let colSpacing = strawberryHeight/7;
    let offset = strawberryWidth/30 // offset for new seed rows;

        for (let j = 1; j <= 5; j ++) {
            ctx.beginPath();
            ctx.ellipse(seedRow,  seeColumn+ 10*Math.random(), seedSize/2, seedSize/1.5, 0, 0, Math.PI * 2);
            ctx.fill();
            seedRow = seedRow + rowSpacing;
        }
        offset = offset +strawberryWidth/20;
        seedRow = centerX- 0.4*strawberryWidth + offset;
        seeColumn = seeColumn + colSpacing;

        for (let j = 1; j <= 4; j ++) {
            ctx.beginPath();
            ctx.ellipse(seedRow,  seeColumn+ 10*Math.random(), seedSize/2, seedSize/1.5, 0, 0, Math.PI * 2);
            ctx.fill();
            seedRow = seedRow + rowSpacing;
        }
        offset = offset +strawberryWidth/20;
        seedRow = centerX- 0.35*strawberryWidth + offset;
        seeColumn = seeColumn + colSpacing;
        for (let j = 1; j <= 3; j ++) {
            ctx.beginPath();
            ctx.ellipse(seedRow,  seeColumn+ 10*Math.random(), seedSize/2, seedSize/1.5, 0, 0, Math.PI * 2);
            ctx.fill();
            seedRow = seedRow + rowSpacing;
        }

        offset = offset +strawberryWidth/20;
        seedRow = centerX- 0.3*strawberryWidth + offset;
        seeColumn = seeColumn + colSpacing;
        for (let j = 1; j <= 2; j ++) {
            ctx.beginPath();
            ctx.ellipse(seedRow,  seeColumn+ 10*Math.random(), seedSize/2, seedSize/1.5, 0, 0, Math.PI * 2);
            ctx.fill();
            seedRow = seedRow + rowSpacing;
        }

    // Green Leaves on top //////////////////////////////////////////////////////////
    ctx.fillStyle = "green";
    ctx.beginPath();
    ctx.moveTo(centerX, bodyY + 0.06*strawberryHeight);
    ctx.ellipse(centerX, bodyY + 0.06*strawberryHeight, strawberryWidth/2, strawberryHeight/13, 0, 0, Math.PI * 2);
    ctx.moveTo(centerX-0.15*strawberryWidth, bodyY + 0.1*strawberryHeight)
    ctx.ellipse(centerX-0.15*strawberryWidth, bodyY + 0.1*strawberryHeight, strawberryWidth/4, strawberryHeight/15, -0.5, 0, Math.PI * 2);
    ctx.moveTo(centerX+0.15*strawberryWidth, bodyY + 0.1*strawberryHeight)
    ctx.ellipse(centerX+0.15*strawberryWidth, bodyY + 0.1*strawberryHeight, strawberryWidth/4, strawberryHeight/15, 0.5, 0, Math.PI * 2);
    ctx.moveTo(centerX, bodyY + 0.05*strawberryHeight)
    ctx.ellipse(centerX, bodyY + 0.05*strawberryHeight, strawberryWidth/4, strawberryHeight/15, 1.5, 0, Math.PI * 2);
    ctx.closePath();
    ctx.fill();

    // Smiley Mouth //////////////////////////////////////////////////////////
    ctx.strokeStyle = "black";
    ctx.fillStyle = "darkred";
    ctx.lineWidth = 1;
    let mouthY = bodyY + strawberryHeight/3.5;
    ctx.beginPath();
    ctx.moveTo(centerX - width/6, mouthY+ strawberryHeight/6);
    ctx.quadraticCurveTo(centerX, mouthY + strawberryHeight/5, centerX + width/6, mouthY + strawberryHeight/6);
    ctx.moveTo(centerX + width/6, mouthY + strawberryHeight/6);
    ctx.quadraticCurveTo(centerX, mouthY + strawberryHeight/3, centerX - width/6, mouthY + strawberryHeight/6);
    ctx.stroke();
    ctx.fill();
    ctx.closePath();

    ctx.fillStyle = "white";
    ctx.beginPath();
    ctx.moveTo(centerX - width/6, mouthY + strawberryHeight/6);
    ctx.quadraticCurveTo(centerX, mouthY + strawberryHeight/5, centerX + width/6, mouthY + strawberryHeight/6);
    ctx.moveTo(centerX + width/6, mouthY + strawberryHeight/6);
    ctx.quadraticCurveTo(centerX, mouthY + strawberryHeight/4, centerX - width/6, mouthY + strawberryHeight/6);
    ctx.fill();
    ctx.closePath();

    // Eyes //////////////////////////////////////////////////////////
    ctx.fillStyle = "white";
    let eyeY = bodyY + strawberryHeight/3.5;
    let eyeWidth = strawberryWidth/12;
    let eyeHeight = strawberryHeight/12;
    ctx.moveTo(centerX - strawberryWidth/5, eyeY);
    ctx.ellipse(centerX - strawberryWidth/5, eyeY, eyeWidth, eyeHeight, 0, 0, Math.PI * 2);
    ctx.moveTo(centerX + strawberryWidth/5, eyeY);
    ctx.ellipse(centerX + strawberryWidth/5, eyeY, eyeWidth, eyeHeight, 0, 0, Math.PI * 2);
    ctx.fill();
    ctx.closePath();

    ctx.fillStyle = "black";
    ctx.beginPath();
    ctx.moveTo(centerX -strawberryWidth/5, eyeY+5);
    ctx.ellipse(centerX -strawberryWidth/5, eyeY+5, 0.6*eyeWidth, 0.7*eyeHeight, 0, 0, Math.PI * 2);
    ctx.moveTo(centerX +strawberryWidth/5, eyeY+5);
    ctx.ellipse(centerX + strawberryWidth/5, eyeY+5, 0.6*eyeWidth, 0.7*eyeHeight, 0, 0, Math.PI * 2);
    ctx.fill();
    ctx.closePath();

    // Curved Arms //////////////////////////////////////////////////////////
    ctx.strokeStyle = "black";
    ctx.lineWidth = width/12;
    let armY = bodyY + strawberryHeight/3.2;
    let stoparmy = strawberryHeight/4;
    ctx.beginPath();
    ctx.moveTo(centerX - 0.45*strawberryWidth, armY);
    ctx.quadraticCurveTo(centerX - 0.7*strawberryWidth, armY + strawberryHeight/6, centerX - 0.5*strawberryWidth, armY + stoparmy);
    ctx.stroke();
    ctx.closePath();

    ctx.beginPath();
    ctx.moveTo(centerX + 0.45*strawberryWidth, armY);
    ctx.quadraticCurveTo(centerX + 0.7*strawberryWidth, armY + strawberryHeight/6, centerX + 0.5*strawberryWidth, armY + stoparmy);
    ctx.stroke();
    ctx.closePath();

    // Curved Legs //////////////////////////////////////////////////////////
    ctx.strokeStyle = "black";
    ctx.lineWidth = width/12;
    let legY = bodyY + 0.55*strawberryHeight;
    ctx.beginPath();
    ctx.moveTo(centerX - strawberryWidth/4.5, legY);
    ctx.quadraticCurveTo(centerX - strawberryWidth/2.5, 0.85*yglobal, centerX - width/3, 0.9*yglobal);
    ctx.stroke();
    ctx.closePath();

    ctx.beginPath();
    ctx.moveTo(centerX + strawberryWidth/4.5, legY);
    ctx.quadraticCurveTo(centerX + strawberryWidth/2.5, 0.85*yglobal, centerX + width/3, 0.9*yglobal);
    ctx.stroke();
    ctx.closePath();

    // Hands (little circles) //////////////////////////////////////////////////////////
    ctx.fillStyle = "salmon";
    ctx.beginPath();
    ctx.ellipse(centerX - 0.5*strawberryWidth, armY + stoparmy, strawberryWidth/10, strawberryWidth/10, 0, 0, Math.PI * 2);
    ctx.ellipse(centerX + 0.5*strawberryWidth, armY + stoparmy, strawberryWidth/10, strawberryWidth/10, 0, 0, Math.PI * 2);
    ctx.fill();

    // Shoes //////////////////////////////////////////////////////////
 // Shoes //////////////////////////////////////////////////////////
 let shoewidth = strawberryWidth/6;
 let shoeheight = strawberryHeight/25;
 let shoeY = 0.9*yglobal;
 let shoeX = width/2.2;

 ctx.fillStyle = "gold"; 
 let goldshoey = 0.91*yglobal; 
 ctx.beginPath();
 ctx.moveTo(centerX - shoeX, goldshoey);
 ctx.ellipse(centerX -shoeX, goldshoey, shoewidth, shoeheight, 0, 0, Math.PI * 2);
 ctx.moveTo(centerX + shoeX, goldshoey);
 ctx.ellipse(centerX + shoeX, goldshoey, shoewidth, shoeheight, 0, 0, Math.PI * 2);
 ctx.fill();
 
 ctx.fillStyle = "salmon";   
 ctx.beginPath();
 ctx.moveTo(centerX - shoeX,shoeY);
 ctx.ellipse(centerX - shoeX, shoeY, shoewidth, shoeheight, 0, 0, Math.PI * 2);
 ctx.moveTo(centerX + shoeX, shoeY);
 ctx.ellipse(centerX + shoeX, shoeY, shoewidth, shoeheight, 0, 0, Math.PI * 2);
 ctx.fill();

}
function strawberryArea(height, width, seedSize) {
  let stxradius = 0.6*width;
  let styradius = 0.63*height;
  // let seedradius = seedSize;
  let totalArea = Math.PI * stxradius * styradius/3;
  //pit size was 0.5*height and 0.5*width
  let SeedArea = 14*4*Math.PI * 0.5*seedSize * seedSize/2;
  let area = Math.round((totalArea - SeedArea)/100);
  if (area < 5) {
    return 5;
  } else{
    return area;
  }
}

function drawMultipleStrawberries(ctx, x, y, heightArray, widthArray, seedSizeArray) {
  for(let i = 0; i < 5; i++) {
      let generation = i * 5;
      let stfruitArea = strawberryArea(heightArray[i], widthArray[i], seedSizeArray[i]);
      // Write the generation number
      ctx.font = '30px Arial';
      ctx.fillStyle = 'black';
      ctx.fillText(`Generation ${generation}`, x/2 - 80, 50);
      ctx.fillText(`Flesh area = ${stfruitArea}`, x/2 - 80, 0.98*y);
      // Draw the maize plant
      drawFunnyStrawberry(ctx, xglobal = x, yglobal = y, heightArray[i],widthArray[i], seedSizeArray[i]);
      x += 580;
    }
}