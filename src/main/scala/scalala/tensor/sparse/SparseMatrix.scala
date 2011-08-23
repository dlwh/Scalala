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
package tensor;
package sparse;


import domain.TableDomain;

import scalala.generic.collection._;
import scalala.scalar.Scalar;
import scalala.library.random.MersenneTwisterFast;

import scalala.operators._;

import org.netlib.blas._;
import org.netlib.lapack._;
import org.netlib.util.intW
import library.{LinearAlgebra, Random}
import collection.sparse.SparseArray
;

/**
 * A SparseMatrix is backed by an array of sparse column vectors.
 *
 * @author dramage
 */
class SparseMatrix[@specialized(Int,Long,Float,Double) V]
(override val numRows: Int, override val numCols: Int, data_ : Array[SparseVectorCol])
(implicit override val scalar : Scalar[V])
extends /* SparseArrayTensor[(Int, Int), V] with SparseArrayTensorLike[(Int, Int), V, TableDomain, SparseMatrix[V]]
with*/ mutable.Matrix[V] with mutable.MatrixLike[V, SparseMatrix[V]] {
  def apply(i: Int, j: Int): V = data_(i)(j)

  def update(i: Int, j: Int, value: V) { data_(i)(j) = value }

  override def foreachNonZeroPair[U](fn: ((Int, Int), V) => U): Boolean = {
    data_.zipWithIndex.foreach {case (sv, r) => sv.foreachNonZeroPair { case (c, v) => fn((r -> c), v)} }
    false;
  }
}