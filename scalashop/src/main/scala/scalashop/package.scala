
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  //in our case restrict between 0 and width-1
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    //println("initial x and y are: ",x,y)
    //var i=x-radius
    //var j=y-radius
    ////I start the i and j with clamp because in case of corner pixels their
    ////initial values will place them out of bounds thus the while loops won't execute
    var i=clamp(x-radius,0, src.width-1)
    var j=clamp(y-radius,0, src.height-1)
    var countPixels=0
    var sumR, sumG, sumB, sumA =0
    def sumChannels(a: Int, b: Int): Int={
      a+b
    }
    def avgChannels(s: Int, c: Int): Int={
      s/c
    }
    //var fruits = new ListBuffer[RGBA]()
    //create a ListBuffer to put the surrounding pixels in
    //println("x and y are ", x, y, radius)
    while (i>=0 && i<=x+radius){
      while (j>=0 && j<=y+radius){
        if (clamp(i,0,src.width-1)==i && clamp(j,0,src.height-1)==j) {
          var pixel=src.apply(i,j)
          countPixels=countPixels+1
          sumR=sumChannels(sumR, red(pixel))
          sumG=sumChannels(sumG, green(pixel))
          sumB=sumChannels(sumB, blue(pixel))
          sumA=sumChannels(sumA, alpha(pixel))
          //println("I am in ",x,y,i,j)
        }
        j=j+1

      }
      //j=y-radius
      ////re-initialization of y essentially, so for the new i (i+1) I will traverse all j values
      j=clamp(y-radius,0, src.height-1)
      i=i+1
    }
    if (countPixels!=0){
      var avgR=avgChannels(sumR,countPixels)
      var avgG=avgChannels(sumG,countPixels)
      var avgB=avgChannels(sumB,countPixels)
      var avgA=avgChannels(sumA,countPixels)
      //println(avgR, avgG, avgB, avgA)
      rgba(avgR, avgG, avgB, avgA)
    }
    else {
      var avgR=0
      var avgG=0
      var avgB=0
      var avgA=0
      rgba(avgR, avgG, avgB, avgA)
    }
    //this is the return value: an RGBA pixels whose channels are the average of surrounding pixels


  }

}
