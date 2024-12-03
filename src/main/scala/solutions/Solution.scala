package solutions

import utils.Utils
trait Solution(input: Seq[String], samp: Boolean):
  def run: Any
  def run2: Any
  def sprint(s: Any*) = if samp then println(s mkString "\t") else ()
  def swrite(s: Any)  = if samp then Utils.write(s) else ()