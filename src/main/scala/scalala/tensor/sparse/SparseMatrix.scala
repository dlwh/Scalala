/*
 * Distributed as part of Scalala, a linear algebra library.
 *
 * Copyright (C) 2008- Daniel Ramage, Matthew Pocock
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
import generic.TensorNonZeroMonadic
import collection.sparse.{DefaultArrayValue}
;

/** Sparse representation of a matrix.
 *
 * `SparseMatrix` is backed by a dense array of `SparseVectorCol`s. This is a good trade-off if each column tends to
 * have a few values.
 *
 * @author Matthew Pocock
 */
class SparseMatrix[@specialized(Int,Long,Float,Double) V]
(override val numRows: Int, override val numCols: Int, data_ : Array[SparseVectorCol[V]])
(implicit override val scalar : Scalar[V])
extends /* SparseArrayTensor[(Int, Int), V] with SparseArrayTensorLike[(Int, Int), V, TableDomain, SparseMatrix[V]]
with*/ mutable.Matrix[V] with mutable.MatrixLike[V, SparseMatrix[V]] {
  def apply(i: Int, j: Int): V = data_(j)(i)

  def update(i: Int, j: Int, value: V) { data_(j)(i) = value }

  override def nonzeroSize = data_.foldLeft(0)(_ + _.nonzeroSize)

//  override def nonzero: TensorNonZeroMonadic[(Int, Int), V, SparseMatrix[V]] =


  override def foreachNonZeroPair[U](fn: ((Int, Int), V) => U): Boolean = {
    // fixme: zipWithIndex may be expensive
    var all = true
    data_.zipWithIndex.foreach {case (sv, j) => all &&= sv.foreachNonZeroPair { case (i, v) => fn((i -> j), v)} }
    all;
  }

  override def foreachNonZeroKey[U](fn: ((Int, Int)) => U): Boolean = {
    // fixme: zipWithIndex may be expensive
    var all = true
    data_.zipWithIndex.foreach {case (sv, j) => all &&= sv.foreachNonZeroKey { i => fn(i -> j)} }
    false;
  }

  override def foreachNonZeroValue[U](fn: (V) => U): Boolean = {
    var all = true
    data_.foreach(all &&= _.foreachNonZeroValue(fn))
    false;
  }

}

object SparseMatrix extends SparseMatrixConstructors {



}

trait SparseMatrixConstructors {
  /** Construct a sparse matrix for the given table domain. */
  def apply[V: Scalar: ClassManifest: DefaultArrayValue](domain: TableDomain) =
    zeros[V](domain._1.size, domain._2.size)

  def apply[R,V](rows: R*)(implicit rl : LiteralRow[R, V], scalar: Scalar[V], mv: ClassManifest[V], dav: DefaultArrayValue[V]) = {
    val nRows = rows.length
    val nCols = rl.length(rows(0))
    val rv = zeros(nRows, nCols);
    for ((row, i) <- rows.zipWithIndex) {
      rl.foreach(row, ((j, v) => if (v != scalar.zero) rv(i, j) = v))
    }
    rv
  }

  def zeros[V](rows: Int, cols: Int)(implicit s: Scalar[V], mv: ClassManifest[V], dav: DefaultArrayValue[V]) = {
    val data = new Array[SparseVectorCol[V]](cols)
    for(c <- 0 until cols) data(c) = SparseVector.zeros[V](rows)
    new SparseMatrix[V](rows, cols, data)
  }
}