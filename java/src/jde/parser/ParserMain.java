package jde.parser;

import jde.parser.syntaxtree.*;
import jde.parser.visitor.*;

public class ParserMain {
  
  public static void main(String args[]) {
    JavaParser parser;
    if ( args.length == 0 ) {
      System.out.println("Java Parser Version 1.1:  Reading from standard input . . .");
      parser = new JavaParser(System.in);
    }
    else
      if ( args.length == 1 ) {
	System.out.println("Java Parser Version 1.1:  Reading from file " + args[0]+ " . . .");
	try
	  {
	    parser = new JavaParser(new java.io.FileInputStream(args[0]));
	  }
	catch (java.io.FileNotFoundException e)
	  {
	    System.out.println("Java Parser Version 1.1:  File " + args[0]+ " not found.");
	    return;
	  }
      }
      else {
	System.out.println("Java Parser Version 1.1:  Usage is one of:");
	System.out.println("         java JavaParser < inputfile");
	System.out.println("OR");
	System.out.println("         java JavaParser inputfile");
	return;
      }
    try {
      parser.CompilationUnit();
      System.out.println("Java Parser Version 1.1:  Java program parsed successfully.");
    }
    catch (ParseException e) {
      System.out.println(e.getMessage());
      System.out.println("Java Parser Version 1.1:  Encountered errors during parse.");
    }
  }

  static JavaParser parser;
  
  public static void parseFile(String pathName)  {
    try {
      if (parser == null)
	parser = new JavaParser(new java.io.FileInputStream(pathName));
      else
	parser.ReInit(new java.io.FileInputStream(pathName));
    }
    catch (java.io.FileNotFoundException e) {
      println("\"Parser error:  File " + pathName + " not found.\"");
      return;
    }

    try {
      parser.CompilationUnit();
      println("nil");
    }
    catch (ParseException e) {
      String message = e.getMessage().replace('\"', '\'');

      println("\"" + message + "\"");
    }
  }
 
  public static void println(String s) {
    System.out.print(s + "\n");
    System.out.flush();
  }

  public static void getVariables(String pathName) {
    try {
      if (parser == null)
	parser = new JavaParser(new java.io.FileInputStream(pathName));
      else
	parser.ReInit(new java.io.FileInputStream(pathName));
    }
    catch (java.io.FileNotFoundException e) {
      println("\"Parser error:  File " + pathName + " not found.\"");
      return;
    }

    try {
      Node root = parser.CompilationUnit();
      println("nil");
      root.accept(new GetVariablesVisitor());

    }
    catch (ParseException e) {
      String message = e.getMessage().replace('\"', '\'');

      println("\"" + message + "\"");
    }
    
  }
  
 
}
