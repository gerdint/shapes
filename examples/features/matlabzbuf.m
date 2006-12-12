function matlabzbuf( )

figure( 1 );

for a = 0 : (2*pi/3) : 6
  leaf( a );
end

patch( [ 3 ; 3 ; 7 ], [ -2 ; 2 ; 0 ], [ 0 ; 0 ; 2 ], 0.3 );
patch( [ 8 ; 8 ; 4 ], [ -1 ; 1 ; 0 ], [ 0 ; 0 ; 2 ], 0.7 );


%%% Uncomment the following to see what the result should look like (except for rasterization)
% set( gcf( ), 'renderer', 'zbuffer' );



%%%

function leaf( a )

x = [ 0.5 ;
      2.5 ;
      1.5 ];
y = [ -4 ;
      -4 ;
      5 ];
z = [ 0 ;
      0 ;
      0.5 ];

patch( x*cos(a)-y*sin(a), x*sin(a)+y*cos(a), z, a / (2*pi));
