package mandelbrot

import java.awt.event.MouseListener
import java.awt.event.MouseEvent
import java.awt.Dimension
import java.awt.Graphics
import java.awt.Color

import javax.swing.JComponent
import javax.swing.JFrame


case class Complex(z1: Double, z2:Double) {
  def *(rhs: Complex) = {
   //(z1+z2i)(w1+w2i) = z1w1-z2w2+(z1w2+w1z2)i
   Complex(z1*rhs.z1-z2*rhs.z2,z1*rhs.z2+rhs.z1*z2)
  }
  def +(rhs: Complex) = {
   //(z1+z2i)+ (w1+w2i) = (z1+w1) + (z2+w2)i
   Complex(z1+rhs.z1,z2+rhs.z2)
  }
  def absVal() = math.sqrt(z1*z1+z2*z2)
  override def toString() = "("+z1+" "+z2+"i)"
}


class MBComponent extends JComponent with MouseListener{
  val iterations = 200
  val frameWidth = 800
  val frameHeight = 400
  val xLimits = (-3.5, 1)
  val yLimits = (-1, 1)
  
  addMouseListener(this)

  def isInSet(c: Complex) : Boolean = {
    var z = Complex(0,0)
    for(i <- 1 to iterations) {
      z = z*z+c
      if(z.absVal > 2) {
        //println(z + " IS NOT in the set")
        return false
      }
    }
    //println(z + " IS in the set")
    true
  }

  override def getPreferredSize() = new Dimension(frameWidth,frameHeight)
  
  override def paintComponent(g: Graphics) = {
    super.paintComponent(g)
    g.setColor(Color.red)
    g.fillRect(0,0,frameWidth, frameHeight)
    for(i <- 0 to frameWidth) {
      val z1 = (i.toDouble/frameWidth)*3.5 - 2.5
      for(j <- 0 to frameHeight) {
        val z2 = (j.toDouble/frameHeight)*2 - 1
        if(isInSet(Complex(z1,z2))) {
            g.setColor(Color.black)
            g.fillRect(i,j,1,1)
        }
      }
    }
  }
  
  def mouseClicked(e: MouseEvent) = {
	  println("Mouse Clicked: " + e)
  }
  
  var mouseDown = (0,0)
  val zooms = Array[(Int,Int)]()
  
  def mousePressed(e: MouseEvent) = {
	  println("Mouse Pressed: " + e)
	  mouseDown = (e.getX(), e.getY())
  }
  
  def mouseReleased(e: MouseEvent) = {
	  println("Mouse Released: " + e)
  }
  
  
  def mouseEntered(e: MouseEvent) = println("Mouse Entered: " + e)
  def mouseExited(e: MouseEvent) = println("Mouse Exited: " + e)
}

object Main {
  def main(argv: Array[String]) = {
	println("Starting...")
    val mainFrame = new JFrame("Mandelbrot Set")
    mainFrame.getContentPane().add(new MBComponent)
    mainFrame.pack()
    mainFrame.setVisible(true)
  }
}


