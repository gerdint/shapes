function [ nNew2, mNew2 ] = transformPlaneEquation( n, m, Trot, Ttrans, testCount )
% [ nNew, mNew ] = transformPlaneEquation( n, m, Trot, Ttrans, testCount )
%
% The original plane equation is
%   x: inner( x, n ) == m
% The transform is
%   x  -->  Trot x + Ttrans
%
% This function computes an equation for the transformed plane.
%
% testCount defaults to 0, and is the number of trial points to generate for validation.


try
  if isempty( n )
    error( 'Use default' );
  end
catch
  n = randn( 3, 1 );
end

try
  if isempty( m )
    error( 'Use default' );
  end
catch
  m = 4;
end

try
  if isempty( Trot )
    error( 'Use default' );
  end
catch
  d = one( 3, 2 );
  delta = 0e-10;   % if zero, the first method fails!
  Trot = ( 1 + delta ) * eye( 3 ) - d * d.';
end

try
  if isempty( Ttrans )
    error( 'Use default' );
  end
catch
  Ttrans = zeros( 3, 1 );
end

try
  if isempty( testCount )
    error( 'Use default' );
  end
catch
  testCount = 0;
end

% The first solution is easy to write, but sensitive to singularities.
%tmp = Trot.' \ n;
tmp = pinv( Trot.' ) * n;
a = 1 / norm( tmp );
nNew1 = a * tmp
mNew1 = nNew1.' * ( Trot * ( n.' \ m ) + Ttrans )

% The second solution is clumsy.
% Take three points in the plane and see where they go!
tmp1 = one( 3, 1 );
tmp2 = one( 3, 2 );
r1 = cross( n, tmp1 );
r2 = cross( n, tmp2 );
if norm( r1 ) > norm( r2 )
  r = r1 / norm( r1 );
else
  r = r2 / norm( r2 );
end
x0 = n.' \ m;
x1 = x0 + r;
x2 = x0 + cross( n / norm( n ), r );
Tx0 = Trot * x0 + Ttrans;
Tx1 = Trot * x1 + Ttrans;
Tx2 = Trot * x2 + Ttrans;
tmp = cross( Tx2 - Tx0, Tx1 - Tx0 );
nNew2 = tmp / norm( tmp )
mNew2 = nNew2.' * Tx0

% The third solution requires the computation of an SVD, but is otherwise the most elegant.
[ U, S, V ] = svd( Trot );
s = diag( S );
if s(2) == 0
  error( 'The transform is too degenerate.' );
end
S2 = diag( [ s(3)/s(1) s(3)/s(2) 1 ] );
itf = U * S2 * V.'
tmp = itf * n;
a = 1 / norm( tmp );
nNew3 = a * tmp
mNew3 = nNew3.' * ( Trot * ( n.' \ m ) + Ttrans )


for i = 1 : testCount
  tmp = randn( 3, 1 );
  x = tmp + ( ( m - n.' * tmp ) / ( n.' * n ) ) * n;
  fprintf( 'Old residual:   %g\n', n.' * x - m );
  fprintf( 'New residual 1: %g\n', nNew1.' * ( Trot * x + Ttrans ) - mNew1 );
  fprintf( 'New residual 2: %g\n', nNew2.' * ( Trot * x + Ttrans ) - mNew2 );
  fprintf( 'New residual 3: %g\n', nNew3.' * ( Trot * x + Ttrans ) - mNew3 );
end
