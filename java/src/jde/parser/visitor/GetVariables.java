package jde.parser.visitor;
import jde.parser.syntaxtree.*;
import java.util.*;

public class GetVariables extends DepthFirstVisitor {



   /**
    * <PRE>
    * f0 -> ( "public" | "protected" | "private" | "static" | "final" | "transient" | "volatile" )*
    * f1 -> Type()
    * f2 -> VariableDeclarator()
    * f3 -> ( "," VariableDeclarator() )*
    * f4 -> ";"
    * </PRE>
    */
   public void visit(FieldDeclaration n) {

     contextFieldDeclaration = true;

     n.f0.accept(this);
     n.f1.accept(this);
     n.f2.accept(this);
     n.f3.accept(this);
     n.f4.accept(this);

     contextFieldDeclaration = false;
   }

   /**
    * <PRE>
    * f0 -> VariableDeclaratorId()
    * f1 -> [ "=" VariableInitializer() ]
    * </PRE>
    */
   public void visit(VariableDeclarator n) {
      n.f0.accept(this);
      n.f1.accept(this);
   }

   /**
    * <PRE>
    * f0 -> &lt;IDENTIFIER&gt;
    * f1 -> ( "[" "]" )*
    * </PRE>
    */
   public void visit(VariableDeclaratorId n) {

     if (contextFieldDeclaration) 
       System.out.println(n.f1);

      n.f0.accept(this);
      n.f1.accept(this);
   }

   /**
    * <PRE>
    * f0 -> ArrayInitializer()
    *       | Expression()
    * </PRE>
    */
   public void visit(VariableInitializer n) {
      n.f0.accept(this);
   }

   /**
    * <PRE>
    * f0 -> "{"
    * f1 -> [ VariableInitializer() ( "," VariableInitializer() )* ]
    * f2 -> [ "," ]
    * f3 -> "}"
    * </PRE>
    */
   public void visit(ArrayInitializer n) {
      n.f0.accept(this);
      n.f1.accept(this);
      n.f2.accept(this);
      n.f3.accept(this);
   }

   /**
    * <PRE>
    * f0 -> [ "final" ]
    * f1 -> Type()
    * f2 -> VariableDeclarator()
    * f3 -> ( "," VariableDeclarator() )*
    * </PRE>
    */
   public void visit(LocalVariableDeclaration n) {
      n.f0.accept(this);
      n.f1.accept(this);
      n.f2.accept(this);
      n.f3.accept(this);
   }


  boolean contextFieldDeclaration = false;
  

  
}
