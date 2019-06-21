delta = 1;

module spring(h, r) {
  color("Silver") linear_extrude(height = h, convexity = 0, twist = -3600) {
    translate([r, 0, 0])
    circle(r = 1);
  }
}

module scraper(x) {
     translate([0, -x/2-delta, -1000]) cube(size = [1000, x+2*delta, 2000]);
}

module outerRoter(h, ro, ri, rp, d, hps, offsets, zs) {
    fn = 10;
    indeces  = [0,1,2,3,4];
    //outer cylinder
    color("Olive") difference() {
       cylinder(r=ro, h=h, center = true, $fn=fn);
       cylinder(r=ri,  h=h+delta, center = true, $fn=fn);
       for (i = zs) {translate([0, 0, i])  rotate([90, 0, 0]) translate([0, 0, ri-delta])  cylinder(r=rp,  h=d, $fn=fn);}
       scraper(1000);
     }
  
  for (i = indeces) {
      translate([0, 0, zs[i]])  rotate([-90, 0, 0]) translate([0, 0, -(ri+d)]) spring((d-hps[i]+offsets[i]), rp);
      translate([0, 0, zs[i]])  rotate([90, 0, 0]) translate([0, 0, ri-offsets[i]]) cylinder(r=rp, h=hps[i]); 
  }
}

module innerRoter(h, r, keyw, keyh, rp, hps, offsets, zs) {
    fn = 10;
    indeces  = [0,1,2,3,4];

    color("Silver") difference() {
       cylinder(r=r,  h=h, center = true, $fn=fn);
       for (i = zs) {translate([0, 0, i]) rotate([90, 0, 0]) cylinder(r=rp,  h=r+delta, $fn=fn);}
       cube(size = [keyw,keyh,h+delta], center = true);
       scraper(400);
    }
    
  color("Red") for (i = indeces) {
      translate([0, 0, zs[i]])  rotate([-90, 0, 0]) translate([0, 0, -r+offsets[i]]) cylinder(r=rp, h=hps[i]); 
  }
}

module key(x, y, z, rp, rp0, ds, zr, zs) {
    fn = 10;
    indeces  = [0,1,2,3,4];
    color("Gold") difference() {
         union() {
           cube(size = [x,y,z], center = true);
           translate([0,0,z/2]) linear_extrude(height = zr, convexity = 10, scale=[0.1,0.1], $fn=100)
 square(size=[x,y], center=true);
         }
         for (i = indeces) {
           translate([0, 0, zs[i]])  rotate([-90, 0, 0]) translate([0, 0, -y/2 - delta])  cylinder(h = ds[i], r1 = rp0, r2 = rp, $fn=fn);
         }
         scraper(100);

     }
}

function th(x) = (exp(x) - exp(-x)) / (exp(x) + exp(-x));
function sigmoid(x, h, u, a) = h * th((x-u) / (2 * a));
function gauss(x, u, s) = exp(- (x-u) * (x-u) / (2*s*s));
function hole(x, h, r, off) = h*gauss(x, off, r/5);

function keyheight(keyy, z, keyds, rpin0, zs) =  keyy/4 - (
        hole(z, keyds[0], rpin0, zs[0]) 
      + hole(z, keyds[1], rpin0, zs[1])
      + hole(z, keyds[2], rpin0, zs[2])
      + hole(z, keyds[3], rpin0, zs[3])
      + hole(z, keyds[4], rpin0, zs[4])
      + sigmoid(z, keyy/4, 325, 15)
      );

module drawKeyLock(x, theta) {
    
  // config
  rinner = 200;
  router = 600;
  keyx = 250;
  keyy = 100;
  keyz = 600;
  keyza= 50;
  rpin  = 20;
  rpin0 = rpin+25;
  outerdhole = 200;
  hspring = 70;
  innerhpins = [180, 170, 190, 210, 170];  
  outerhpins = [120, 130, 140, 130, 130]; // < outerdhole
  indeces  = [0,1,2,3,4];
  
  //key
  keyds  =  [ for (i = indeces) let (l = innerhpins[i])
      keyy/2 - rinner + l
  ];
  //echo(keyds);
  //dynamics
  zs = [-200, -100, 0, 100, 200];
  hlpin = [ for (i = indeces) let (l = innerhpins[i], u = outerhpins[i] )
      max(keyheight(keyy, zs[i]-x, keyds, rpin0, zs), rinner + outerdhole - l - u - hspring)
  ];
  offsets = [ for (i = indeces) let (l = innerhpins[i], h = hlpin[i] )
      rinner - (h + l)
  ];
  
  //render
  outerRoter(keyz, router, rinner, rpin, outerdhole, outerhpins, offsets, zs);
  rotate([0, 0, theta])  innerRoter(keyz, rinner, keyx, keyy, rpin, innerhpins, offsets, zs);
  rotate([0, 0, theta]) translate([0, 0, x]) key(keyx, keyy, keyz, rpin, rpin0, keyds, keyza, zs);
  
  /* 
  //debug
  z = 800*$t - 400;
  echo(sigmoid(550, 25, 300, 10));
  key(keyx, keyy, keyz, rpin, rpin0, keyds, keyza, zs);
  y = keyheight(keyy, z, keyds, rpin0, zs);
  #translate([0, 0, z]) rotate([90, 0, 0]) translate([0, 0, y]) cylinder(r=20,  h=100);
   */
  
}
x = min(0, -650 + 850 * $t);
theta = max(0, -810 +  900 * $t);
drawKeyLock(x, theta);


$vpr = [0, 240, 180];
$vpd = 3000;
$vpt  = [0, 0, 0];
