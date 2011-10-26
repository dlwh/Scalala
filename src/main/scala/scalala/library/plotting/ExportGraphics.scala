/*
 * Distributed as part of Scalala, a linear algebra library.
 *
 * Copyright (C) 2008- Daniel Ramage
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA
 */
package scalala;
package library;
package plotting;

import java.io.{File,OutputStream,FileOutputStream,IOException};
import java.awt.Graphics2D;

/**
 * Utility functions for exporting a Graphics2D drawable to some eps, pdf,
 * and png.
 *
 * @author dramage, Robby McKilliam
 */
object ExportGraphics {
  /** A Drawable is any function that draws to a Graphics2D context. */
  type Drawable = ((Graphics2D)=>Unit);

  /**
   * Writes the given drawable to a new file of the given name with
   * the given dpi (ignored for PDF).  The extension of the file
   * determines its format, with options png, eps, and pdf.
   */
  def writeFile(file : File, draw : Drawable, width : Int, height : Int, dpi : Int = 72) = {
    lazy val fos = new FileOutputStream(file);
    if (file.getName.toLowerCase.endsWith(".png")) {
      try {
        writePNG(fos,draw,width,height,dpi);
      } finally {
        fos.close();
      }
    } else if (file.getName.toLowerCase.endsWith(".eps")) {
      try {
        writeEPS(fos,draw,width,height,dpi);
      } finally {
        fos.close();
      }
    } else if (file.getName.toLowerCase.endsWith(".pdf")) {
      try {
        writePDF(fos,draw,width,height);
      } finally {
        fos.close();
      }
    } else {
      throw new IOException("Unrecognized file extension: should be png, eps, or pdf");
    }
  }

  /** 
   * Writes the given drawable to the given OutputStream at the given dpi,
   * formatted as png.
   */
  def writePNG(out : OutputStream, draw : Drawable, width : Int, height : Int, dpi : Int = 72) {
    import javax.imageio.ImageIO;
    import java.awt.image.BufferedImage;
    
    // default dpi is 72
    val scale = dpi / 72.0;
    val swidth = (width * scale).toInt;
    val sheight = (height * scale).toInt;

    val image = new BufferedImage(swidth,sheight,BufferedImage.TYPE_INT_ARGB);
    val g2d = image.createGraphics();
    g2d.scale(scale, scale);
    draw(g2d);
    g2d.dispose;

    ImageIO.write(image, "png", out);
  }

  /**
   * Writes the given drawable to the given OutputStream at the given dpi,
   * formatted as eps.
   */
  def writeEPS(out : OutputStream, draw : Drawable, width : Int, height : Int, dpi : Int = 72) {
    import org.apache.xmlgraphics.java2d.ps.EPSDocumentGraphics2D;
    import org.apache.xmlgraphics.java2d.GraphicContext;
    
    // default dpi is 72
    val scale = dpi / 72.0;
    val swidth = (width * scale).toInt;
    val sheight = (height * scale).toInt;

    // an attempt at getting the renderer to do high-res correctly. failed.
    /*
    for (plot <- plots) {
      import java.awt.RenderingHints._;
      val hints = new java.awt.RenderingHints(KEY_DITHERING, VALUE_DITHER_DISABLE);
      for (pair <- List((KEY_ANTIALIASING, VALUE_ANTIALIAS_OFF),
                        (KEY_STROKE_CONTROL, VALUE_STROKE_PURE),
                        (KEY_TEXT_ANTIALIASING, VALUE_TEXT_ANTIALIAS_OFF))) {
        hints.put(pair._1,pair._2);
      }
      plot.chart.setRenderingHints(hints);
    }
    */

    // create an eps document at the appropriate dpi
    val g2d = new EPSDocumentGraphics2D(false);
    g2d.setGraphicContext(new GraphicContext);
    g2d.setupDocument(out, swidth, sheight);
    g2d.scale(scale, scale);
    draw(g2d);
    g2d.finish();
  }

  /**
   * Writes the given drawable to the given OutputStream at the given dpi,
   * formatted as pdf. Contributed by Robby McKilliam.
   */
  def writePDF(out : OutputStream, draw : Drawable, width : Int, height : Int) {
    import com.lowagie.text.Document;
    import com.lowagie.text.Rectangle;
    import com.lowagie.text.pdf.PdfWriter;
  
    val document = new Document();

    try {
      document.setPageSize(new Rectangle(width, height));
      val writer = PdfWriter.getInstance(document, out);
      document.open();

      val cb = writer.getDirectContent();
      val tp = cb.createTemplate(width, height);
      val g2d = tp.createGraphics(width, height);

      draw(g2d);

      g2d.dispose;

      cb.addTemplate(tp, 1, 0, 0, 1, 0, 0);
    } finally {
      document.close();
    }
  }
}
