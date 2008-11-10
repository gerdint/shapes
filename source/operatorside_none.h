/* This file is part of Shapes.
 *
 * Shapes is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * Shapes is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Shapes.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2008 Henrik Tidefelt
 */

/* Use the two looping macros above in a nested manner to produce a default method declaration
 * for each pair combination of classes.
 */
#define DEFAULTUNARYOPDECL_( Ta ) virtual RefCountPtr< const Lang::Value > op( const RefCountPtr< const Shapes::Ta > & arg ) const;
#define DEFAULTUNARYOPDECL FORALLCLASSESM( DEFAULTUNARYOPDECL_ )

/* ... and an implementation.
 */
#define DEFAULTUNARYOPIMPL_( Ts, Ta ) RefCountPtr< const Lang::Value > Shapes::Ts::op( const RefCountPtr< const Shapes::Ta > & arg ) const { return this->throwNotApplicable( arg.getPtr( ) ); }
#define DEFAULTUNARYOPIMPL( Ts ) FORALLCLASSESMT( DEFAULTUNARYOPIMPL_, Ts )

/* Define a macro for calling the implementation of an unary operator.
 */

#define UNARYCALLIMPL( T )



/* Use the two looping macros above in a nested manner to produce a default method declaration
 * for each pair combination of classes.
 */
#define DEFAULTBINARYOPDECL__( Ta, Tb ) virtual RefCountPtr< const Lang::Value > op( const RefCountPtr< const Shapes::Ta > & arg1, const RefCountPtr< const Shapes::Tb > & arg2 ) const;
#define DEFAULTBINARYOPDECL_( Ta ) FORALLCLASSESMT( DEFAULTBINARYOPDECL__, Ta )
#define DEFAULTBINARYOPDECL FORALLCLASSESM( DEFAULTBINARYOPDECL_ )

/* ... and an implementation.
 */
#define DEFAULTBINARYOPIMPL__( Ta, Tb ) RefCountPtr< const Lang::Value > Ast::BinaryInfixExpr::op( const RefCountPtr< const Shapes::Ta > & arg1, const RefCountPtr< const Shapes::Tb > & arg2 ) const { return throwNotApplicable( arg1.getPtr( ), arg2.getPtr( ) ); }
#define DEFAULTBINARYOPIMPL_( Ta ) FORALLCLASSESMT( DEFAULTBINARYOPIMPL__, Ta )
#define DEFAULTBINARYOPIMPL FORALLCLASSESM( DEFAULTBINARYOPIMPL_ )

/* Define a macro for calling the implementation of a binary operator.
 */
#define CALLIMPL( Sa, Sb )


