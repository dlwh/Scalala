/*
 * Distributed as part of Scalala, a linear algebra library.
 *
 * Copyright (C) 2008- Daniel Ramage
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA
 */
package scalala;
package library;

import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.scalatest.matchers.ShouldMatchers;
import org.junit.runner.RunWith;

import scalala.tensor.mutable.{Matrix,Vector}
import scalala.library.LinearAlgebra._;

@RunWith(classOf[JUnitRunner])
class LinearAlgebraTest extends FunSuite with Checkers with ShouldMatchers {
  test("kron") {
    val a = Matrix((1,2),(3,4));
    val b = Matrix((0,5),(6,7));
    assert(kron(a,b) === Matrix((0,5,0,10),(6,7,12,14),(0,15,0,20),(18,21,24,28)));
  }

  test("ranks") {
    assert(ranks(Array(1,2,3)).toList  === List(1.0,2.0,3.0));
    assert(ranks(Array(3,-1,2)).toList === List(3.0,1.0,2.0));
    assert(ranks(Array(1,2,3,3)).toList === List(1.0,2.0,3.5,3.5));
    assert(ranks(Array(1,2,3,3,3)).toList === List(1.0,2.0,4.0,4.0,4.0));
  }

  test("cholesky") {
    val A = Matrix((1.,0.,0.),(2.,3.,0.),(4.,5.,6.))
    val Sigma = A * A.t
    assert(cholesky(Sigma) === A)
  }

  test("eigSym") {
    val A = Matrix((9.,0.,0.),(0.,82.,0.),(0.,0.,25.))
    val (lambda, Some(evs)) = eigSym(A, true)
    assert(lambda === Vector(9.,25.,82.))
    assert(evs === Matrix((1.,0.,0.),(0.,0.,1.),(0.,1.,0.)))
  }

  test("LUfactorization") {
    val (m, _) = LU(Matrix(( 29, 42, -4, 50, 1),
                           ( 20,-31, 32, 21, 2),
                           (-47,-20, 24,-22, 3),
                           (  3, 17,-45, 23, 4)))
    val aux = Matrix((-47.0000, -20.0000, 24.0000, -22.0000, 3.0000),
                     ( -0.4255, -39.5106, 42.2127,  11.6382, 3.2765),
                     ( -0.6170,  -0.7506, 42.4964,  45.1620, 5.3107),
                     ( -0.0638,  -0.3979, -0.6275,  54.5694, 8.8282))
    assert(m forall ((idx,v) => math.abs(v-aux(idx)) < 1e-4))
  }

  test("det") {
    val A = Matrix((9,26,21),(48,3,11),(7,48,26))
    det(A) should be (13446.99999999 plusOrMinus 1e-8)

    val B = Matrix((1,2,3),(4,5,-6),(7,8,9))
    det(B) should be (-72.0 plusOrMinus 1e-15)

    val C = Matrix((1,2,3),(2,4,6),(0,-1,0)) // 1st and 2nd row linearly dep.
    det(C) should be (0.0 plusOrMinus 1e-15)
  }

  test("inv") {
    val X = Matrix(( 29, 42, -4, 50),
                   ( 20,-31, 32, 21),
                   (-47,-20, 24,-22),
                   (  3, 17,-45, 23))
    val I = Matrix.eye[Double](4)
    assert((inv(X) * X) forall ((idx,v) => math.abs(v-I(idx)) < 1e-15))
  }

  test("pinv") {
    val X = Matrix((54, 95), (23, 25), (70, 41), (31, 19))
    val I = Matrix.eye[Double](2)
    assert((pinv(X) * X) forall ((idx,v) => math.abs(v-I(idx)) < 1e-15))
  }

}
