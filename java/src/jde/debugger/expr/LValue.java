/*
 * @(#)LValue.java	1.17 99/05/21
 *
 * Copyright (c) 1997-1999 by Sun Microsystems, Inc. All Rights Reserved.
 * 
 * Sun grants you ("Licensee") a non-exclusive, royalty free, license to use,
 * modify and redistribute this software in source and binary code form,
 * provided that i) this copyright notice and license appear on all copies of
 * the software; and ii) Licensee does not utilize the software in a manner
 * which is disparaging to Sun.
 * 
 * This software is provided "AS IS," without a warranty of any kind. ALL
 * EXPRESS OR IMPLIED CONDITIONS, REPRESENTATIONS AND WARRANTIES, INCLUDING ANY
 * IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE OR
 * NON-INFRINGEMENT, ARE HEREBY EXCLUDED. SUN AND ITS LICENSORS SHALL NOT BE
 * LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING
 * OR DISTRIBUTING THE SOFTWARE OR ITS DERIVATIVES. IN NO EVENT WILL SUN OR ITS
 * LICENSORS BE LIABLE FOR ANY LOST REVENUE, PROFIT OR DATA, OR FOR DIRECT,
 * INDIRECT, SPECIAL, CONSEQUENTIAL, INCIDENTAL OR PUNITIVE DAMAGES, HOWEVER
 * CAUSED AND REGARDLESS OF THE THEORY OF LIABILITY, ARISING OUT OF THE USE OF
 * OR INABILITY TO USE SOFTWARE, EVEN IF SUN HAS BEEN ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGES.
 * 
 * This software is not designed or intended for use in on-line control of
 * aircraft, air traffic, aircraft navigation or aircraft communications; or in
 * the design, construction, operation or maintenance of any nuclear
 * facility. Licensee represents and warrants that it will not use or
 * redistribute the Software for such purposes.
 */

package jde.debugger.expr;

import com.sun.jdi.*;
import java.util.*;

abstract class LValue {

    abstract Value getValue() throws InvocationException, 
                                     IncompatibleThreadStateException,
                                     InvalidTypeException,
                                     ClassNotLoadedException;

    abstract void setValue0(Value value) 
                   throws ParseException, InvalidTypeException, 
                          ClassNotLoadedException;

    abstract void invokeWith(List arguments) throws ParseException;

    void setValue(Value value) throws ParseException {
        try {
            setValue0(value);
        } catch (InvalidTypeException exc) {
            throw new ParseException(
                "Attempt to set value of incorrect type" +
                exc);
        } catch (ClassNotLoadedException exc) {
            throw new ParseException(
                "Attempt to set value before " + exc.className() + " was loaded" +
                exc);
        }
    }        

    void setValue(LValue lval) throws ParseException {
        setValue(lval.interiorGetValue());
    }

    LValue memberLValue(ExpressionParser.GetFrame frameGetter, 
                        String fieldName) throws ParseException {
        try {
            return memberLValue(fieldName, frameGetter.get().thread());
        } catch (IncompatibleThreadStateException exc) {
            throw new ParseException("Thread not suspended");
        }
    }

    LValue memberLValue(String fieldName, ThreadReference thread) throws ParseException {
        return new LValueInstanceMember(interiorGetValue(), fieldName, thread);
    }

    Value interiorGetValue() throws ParseException {
        Value value;
        try {
            value = getValue();
        } catch (InvocationException e) {
            throw new ParseException("Unable to complete expression. Exception " +
                                     e.exception() + " thrown");
        } catch (IncompatibleThreadStateException itse) {
            throw new ParseException("Unable to complete expression. Thread " +
                                     "not suspended for method invoke");
        } catch (InvalidTypeException ite) {
            throw new ParseException("Unable to complete expression. Method " +
                                     "argument type mismatch");
        } catch (ClassNotLoadedException tnle) {
            throw new ParseException("Unable to complete expression. Method " +
                                     "argument type " + tnle.className() + 
                                     " not yet loaded");
        }
//        if (value == null) {
//            throw new ParseException("Cannot invoke a void method within an expression");
//        }
        return value;
    }

    LValue arrayElementLValue(LValue lval) throws ParseException {
        Value indexValue = lval.interiorGetValue();
        int index;
        if ( (indexValue instanceof IntegerValue) ||
             (indexValue instanceof ShortValue) ||
             (indexValue instanceof ByteValue) ||
             (indexValue instanceof CharValue) ) {
            index = ((PrimitiveValue)indexValue).intValue();
        } else {
            throw new ParseException("Array index must be a integer type");
        }
        return new LValueArrayElement(interiorGetValue(), index);
    }

    public String toString() {
        try {
            return interiorGetValue().toString();
        } catch (ParseException e) {
            return "<Parse Exception>";
        }
    }

    static final int STATIC = 0;
    static final int INSTANCE = 1;

    static Field fieldByName(ReferenceType refType, String name, int kind) {
        /*
         * TO DO: Note that this currently fails to find superclass
         * or implemented interface fields. This is due to a temporary
         * limititation of RefType.fieldByName. Once that method is 
         * fixed, superclass fields will be found.
         */
        Field field = refType.fieldByName(name);
        if (field != null) {
            boolean isStatic = field.isStatic();
            if (((kind == STATIC) && !isStatic) || 
                ((kind == INSTANCE) && isStatic)) {
                field = null;
            }
        }
/***
        System.err.println("fieldByName: " + refType.name() + " " +
                                             name + " " +
                                             kind + " " + 
                                             (field != null));
***/
        return field;
    }

    static List methodsByName(ReferenceType refType, String name, int kind) {
        List list = refType.methodsByName(name);
        Iterator iter = list.iterator();
        while (iter.hasNext()) {
            Method method = (Method)iter.next();
            boolean isStatic = method.isStatic();
            if (((kind == STATIC) && !isStatic) || 
                ((kind == INSTANCE) && isStatic)) {
                iter.remove();
            }
        }
        return list;
    }

    static List primitiveTypeNames = new ArrayList();
    static {
        primitiveTypeNames.add("boolean");
        primitiveTypeNames.add("byte");
        primitiveTypeNames.add("char");
        primitiveTypeNames.add("short");
        primitiveTypeNames.add("int");
        primitiveTypeNames.add("long");
        primitiveTypeNames.add("float");
        primitiveTypeNames.add("double");
    }

    static boolean argumentsMatch(List argNames, List arguments) {
        if (argNames.size() != arguments.size()) {
            return false;
        }
        Iterator nameIter = argNames.iterator();
        Iterator valIter = arguments.iterator();
        while (nameIter.hasNext()) {
            String argTypeName = (String)nameIter.next();
            Value value = (Value)valIter.next();
            /*
             * For now we require exact match
             */
            if (value == null) {
                // Null values can be passed to any non-primitive argument
                if (primitiveTypeNames.contains(argTypeName)) {
                    return false;
                }
            } else if (!argTypeName.equals(value.type().name())) {
                return false;
            }
        }
        return true;
    }

    static Method resolveOverload(List overloads, List arguments) 
                                       throws ParseException {
        Iterator iter = overloads.iterator();
        while (iter.hasNext()) {
            Method method = (Method)iter.next();
            List argNames = method.argumentTypeNames();
            if (argumentsMatch(argNames, arguments)) {
                return method;
            }
        }
        throw new ParseException("Arguments match no method");
    }

    private static class LValueLocal extends LValue {
        final StackFrame frame;
        final LocalVariable var;

        LValueLocal(StackFrame frame, LocalVariable var) {
            this.frame = frame;
            this.var = var;
        }
        
        Value getValue() {
            return frame.getValue(var);
        }

        void setValue0(Value val) throws InvalidTypeException, 
                                         ClassNotLoadedException {
            frame.setValue(var, val);
        }

        void invokeWith(List arguments) throws ParseException {
            throw new ParseException(var.name() + " is not a method");
        }
    }

    private static class LValueInstanceMember extends LValue {
        final ObjectReference obj;
        final ThreadReference thread;
        final Field matchingField;
        final List overloads;
        Method matchingMethod = null;
        List methodArguments = null;

        LValueInstanceMember(Value value, 
                            String memberName, 
                            ThreadReference thread) throws ParseException {
            if (!(value instanceof ObjectReference)) {
                throw new ParseException(
                       "Cannot access field of primitive type: " + value);
            }
            this.obj = (ObjectReference)value;
            this.thread = thread;
            ReferenceType refType = obj.referenceType();
            /*
             * Can't tell yet whether this LValue will be accessed as a
             * field or method, so we keep track of all the possibilities
             */
            matchingField = LValue.fieldByName(refType, memberName, 
                                               LValue.INSTANCE);
            overloads = LValue.methodsByName(refType, memberName,
                                              LValue.INSTANCE);
            if ((matchingField == null) && overloads.size() == 0) {
                throw new ParseException("No instance field or method with the name "
                               + memberName + " in " + refType.name());
            }
        }
        
        Value getValue() throws InvocationException, InvalidTypeException,
                                ClassNotLoadedException, IncompatibleThreadStateException {
            if (matchingMethod == null) {
                return obj.getValue(matchingField);
            } else {
                return obj.invokeMethod(thread, matchingMethod, methodArguments, 0);
            }
        }

        void setValue0(Value val) throws ParseException, 
                                         InvalidTypeException,
                                         ClassNotLoadedException {
            if (matchingMethod != null) {
                throw new ParseException("Cannot assign to a method invocation");
            }
            obj.setValue(matchingField, val);
        }

        void invokeWith(List arguments) throws ParseException {
            if (matchingMethod != null) {
                throw new ParseException("Invalid consecutive invocations");
            }
            methodArguments = arguments;
            matchingMethod = LValue.resolveOverload(overloads, arguments);
        }
    }

    private static class LValueStaticMember extends LValue {
        final ReferenceType refType;
        final ThreadReference thread;
        final Field matchingField;
        final List overloads;
        Method matchingMethod = null;
        List methodArguments = null;

        LValueStaticMember(ReferenceType refType, 
                          String memberName,
                          ThreadReference thread) throws ParseException {
            this.refType = refType;
            this.thread = thread;
            /*
             * Can't tell yet whether this LValue will be accessed as a
             * field or method, so we keep track of all the possibilities
             */
            matchingField = LValue.fieldByName(refType, memberName, 
                                               LValue.STATIC);
            overloads = LValue.methodsByName(refType, memberName,
                                              LValue.STATIC);
            if ((matchingField == null) && overloads.size() == 0) {
                throw new ParseException("No static field or method with the name "
                               + memberName + " in " + refType.name());
            }
        }
        
        Value getValue() throws InvocationException, InvalidTypeException,
                                ClassNotLoadedException, IncompatibleThreadStateException {
            if (matchingMethod == null) {
                return refType.getValue(matchingField);
            } else if (refType instanceof ClassType) {
                ClassType clazz = (ClassType)refType;
                return clazz.invokeMethod(thread, matchingMethod, methodArguments, 0);
            } else {
                throw new InvalidTypeException("Cannot invoke static method on " +
                                         refType.name());
            }
        }

        void setValue0(Value val) 
                           throws ParseException, InvalidTypeException,
                                  ClassNotLoadedException {
            if (matchingMethod != null) {
                throw new ParseException("Cannot assign to a method invocation");
            }
            if (!(refType instanceof ClassType)) {
                throw new ParseException(
                       "Cannot set interface field: " + refType);
            }
            ((ClassType)refType).setValue(matchingField, val);
        }

        void invokeWith(List arguments) throws ParseException {
            if (matchingMethod != null) {
                throw new ParseException("Invalid consecutive invocations");
            }
            methodArguments = arguments;
            matchingMethod = LValue.resolveOverload(overloads, arguments);
        }
    }

    private static class LValueArrayElement extends LValue {
        final ArrayReference array;
        final int index;

        LValueArrayElement(Value value, int index) throws ParseException {
            if (!(value instanceof ArrayReference)) {
                throw new ParseException(
                       "Must be array type: " + value);
            }
            this.array = (ArrayReference)value;
            this.index = index;
        }
        
        Value getValue() {
            return array.getValue(index);
        }

        void setValue0(Value val) throws InvalidTypeException, 
                                         ClassNotLoadedException  {
            array.setValue(index, val);
        }

        void invokeWith(List arguments) throws ParseException {
            throw new ParseException("Array element is not a method");
        }
    }

    private static class LValueConstant extends LValue {
        final Value value;

        LValueConstant(Value value) {
            this.value = value;
        }
        
        Value getValue() {
            return value;
        }

        void setValue0(Value val) throws ParseException {
            throw new ParseException("Cannot set constant: " + value);
        }

        void invokeWith(List arguments) throws ParseException {
            throw new ParseException("Constant is not a method");
        }
    }

    static LValue make(VirtualMachine vm, boolean val) {
        return new LValueConstant(vm.mirrorOf(val));
    }

    static LValue make(VirtualMachine vm, byte val) {
        return new LValueConstant(vm.mirrorOf(val));
    }

    static LValue make(VirtualMachine vm, char val) {
        return new LValueConstant(vm.mirrorOf(val));
    }

    static LValue make(VirtualMachine vm, short val) {
        return new LValueConstant(vm.mirrorOf(val));
    }

    static LValue make(VirtualMachine vm, int val) {
        return new LValueConstant(vm.mirrorOf(val));
    }

    static LValue make(VirtualMachine vm, long val) {
        return new LValueConstant(vm.mirrorOf(val));
    }

    static LValue make(VirtualMachine vm, float val) {
        return new LValueConstant(vm.mirrorOf(val));
    }

    static LValue make(VirtualMachine vm, double val) {
        return new LValueConstant(vm.mirrorOf(val));
    }

    static LValue make(VirtualMachine vm, String val) throws ParseException {
        return new LValueConstant(vm.mirrorOf(val));
    }

    static LValue makeBoolean(VirtualMachine vm, Token token) {
        return make(vm, token.image.charAt(0) == 't');
    }

    static LValue makeCharacter(VirtualMachine vm, Token token) {
        return make(vm, token.image.charAt(1));
    }

    static LValue makeFloat(VirtualMachine vm, Token token) {
        return make(vm, Float.valueOf(token.image).floatValue());
    }

    static LValue makeDouble(VirtualMachine vm, Token token) {
        return make(vm, Double.valueOf(token.image).doubleValue());
    }

    static LValue makeInteger(VirtualMachine vm, Token token) {
        return make(vm, Integer.parseInt(token.image));
    }

    static LValue makeShort(VirtualMachine vm, Token token) {
        return make(vm, Short.parseShort(token.image));
    }

    static LValue makeLong(VirtualMachine vm, Token token) {
        return make(vm, Long.parseLong(token.image));
    }

    static LValue makeByte(VirtualMachine vm, Token token) {
        return make(vm, Byte.parseByte(token.image));
    }

    static LValue makeString(VirtualMachine vm, 
                             Token token) throws ParseException {
        int len = token.image.length();
        return make(vm, token.image.substring(1,len-1));
    }

    static LValue makeNull(VirtualMachine vm, 
                           Token token) throws ParseException {
        return new LValueConstant(null);
    }

    static LValue makeThisObject(VirtualMachine vm, 
                                 ExpressionParser.GetFrame frameGetter, 
                                 Token token) throws ParseException {
        if (frameGetter == null) {
            throw new ParseException("No current thread");
        } else {
            try {
                StackFrame frame = frameGetter.get();
                ObjectReference thisObject = frame.thisObject();
                if (thisObject == null) {
                    throw new ParseException(
                        "No 'this'.  In native or static method");
                } else {
                    return new LValueConstant(thisObject);
                }
            } catch (IncompatibleThreadStateException exc) {
                throw new ParseException("Thread not suspended");
            }
        }
    }

    static LValue makeNewObject(VirtualMachine vm, 
                                 ExpressionParser.GetFrame frameGetter, 
                                String className, List arguments) throws ParseException {
        List classes = vm.classesByName(className);
        if (classes.size() == 0) {
            throw new ParseException("No class named: " + className);
        }

        if (classes.size() > 1) {
            throw new ParseException("More than one class named: " +
                                     className);
        }
        ReferenceType refType = (ReferenceType)classes.get(0);


        if (!(refType instanceof ClassType)) {
            throw new ParseException("Cannot create instance of interface " +
                                     className);
        }

        ClassType classType = (ClassType)refType;
        List methods = new ArrayList(classType.methods()); // writable
        Iterator iter = methods.iterator();
        while (iter.hasNext()) {
            Method method = (Method)iter.next();
            if (!method.isConstructor()) {
                iter.remove();
            }
        }
        Method constructor = LValue.resolveOverload(methods, arguments);

        ObjectReference newObject;
        try {
            ThreadReference thread = frameGetter.get().thread();
            newObject = classType.newInstance(thread, constructor, arguments, 0);
        } catch (InvocationException ie) {
            throw new ParseException("Exception in " + className + " constructor: " + 
                                     ie.exception().referenceType().name());
        } catch (IncompatibleThreadStateException exc) {
            throw new ParseException("Thread not suspended");
        } catch (Exception e) {
            /*
             * TO DO: Better error handling
             */
            throw new ParseException("Unable to create " + className + " instance");
        }
        return new LValueConstant(newObject);
    }

    private static LValue nFields(LValue lval, 
                                  StringTokenizer izer,
                                  ThreadReference thread) 
                                          throws ParseException {
        if (!izer.hasMoreTokens()) {
            return lval;
        } else {
            return nFields(lval.memberLValue(izer.nextToken(), thread), izer, thread);
        }                    
    }

    static LValue makeName(VirtualMachine vm, 
                           ExpressionParser.GetFrame frameGetter, 
                           String name) throws ParseException {
        StringTokenizer izer = new StringTokenizer(name, ".");
        String first = izer.nextToken();

        // check local variables
        if (frameGetter != null) {
            try {
                StackFrame frame = frameGetter.get();
                ThreadReference thread = frame.thread();
                LocalVariable var;
                try {
                    var = frame.visibleVariableByName(first);
                } catch (AbsentInformationException e) {
                    var = null;
                }
                if (var != null) {
                    return nFields(new LValueLocal(frame, var), izer, thread);
                } else {
                    ObjectReference thisObject = frame.thisObject();
                    if (thisObject != null) {
                        // check if it is a field of 'this'
                        LValue thisLValue = new LValueConstant(thisObject);
                        LValue fv;
                        try {
                            fv = thisLValue.memberLValue(first, thread);
                        } catch (ParseException exc) {
                            fv = null;
                        }
                        if (fv != null) {
                            return nFields(fv, izer, thread);
                        }
                    }
                }
                // check for class name
                while (izer.hasMoreTokens()) {
                    List classes = vm.classesByName(first);
                    if (classes.size() > 0) {
                        if (classes.size() > 1) {
                            throw new ParseException("More than one class named: " +
                                                     first);
                        } else {
                            ReferenceType refType = (ReferenceType)classes.get(0);
                            LValue lval = new LValueStaticMember(refType, 
                                                            izer.nextToken(), thread);
                            return nFields(lval, izer, thread);
                        }
                    }
                    first = first + '.' + izer.nextToken();
                }
            } catch (IncompatibleThreadStateException exc) {
                throw new ParseException("Thread not suspended");
            }
        }
        throw new ParseException("Name unknown: " + name);
    }

    static String stringValue(Value val) {
        if (val instanceof StringReference) {
            return ((StringReference)val).value();
        } else if (val instanceof ObjectReference) {
            return ((ObjectReference)val).toString();  // TODO
        } else if (val == null) {
            return "null";  
        } else {
            return val.toString();  // TODO not correct in all cases
        }
    }

    static LValue booleanOperation(VirtualMachine vm, Token token, 
                            LValue rightL, 
                            LValue leftL) throws ParseException {
        String op = token.image;
        Value right = rightL.interiorGetValue();
        Value left = leftL.interiorGetValue();
        if ( !(right instanceof PrimitiveValue) || 
             !(left instanceof PrimitiveValue) ) {
            if (op.equals("==")) {
                return make(vm, right.equals(left));
            } else if (op.equals("!=")) {
                return make(vm, !right.equals(left));
            } else {
                throw new ParseException("Operands or '" + op + 
                                     "' must be primitive");
            }
        }
        // can compare any numeric doubles
        double rr = ((PrimitiveValue)right).doubleValue();
        double ll = ((PrimitiveValue)left).doubleValue();
        boolean res;
        if (op.equals("<")) {
            res = rr < ll;
        } else if (op.equals(">")) {
            res = rr > ll;
        } else if (op.equals("<=")) {
            res = rr <= ll;
        } else if (op.equals(">=")) {
            res = rr >= ll;
        } else if (op.equals("==")) {
            res = rr == ll;
        } else if (op.equals("!=")) {
            res = rr != ll;
        } else {
            throw new ParseException("Unknown operation: " + op);
        }
        return make(vm, res);
    }                                

    static LValue operation(VirtualMachine vm, Token token, 
                            LValue rightL, 
                            LValue leftL) throws ParseException {
        String op = token.image;
        Value right = rightL.interiorGetValue();
        Value left = leftL.interiorGetValue();
        if ((right instanceof StringReference) ||
                              (left instanceof StringReference)) {
            if (op.equals("+")) {
                return make(vm, stringValue(right) + stringValue(left));
            }
        }
        if ((right instanceof ObjectReference) ||
                              (left instanceof ObjectReference)) {
            if (op.equals("==")) {
                return make(vm, right.equals(left));
            } else if (op.equals("!=")) {
                return make(vm, !right.equals(left));
            } else {
                throw new ParseException("Invalid operation '" +
                                         op + "' on an Object");
            }
        }
        if ((right instanceof BooleanValue) ||
                              (left instanceof BooleanValue)) {
            throw new ParseException("Invalid operation '" +
                                     op + "' on a Boolean");
        }
        // from here on, we know it is a integer kind of type
        PrimitiveValue primRight = (PrimitiveValue)right;
        PrimitiveValue primLeft = (PrimitiveValue)left;
        if ((primRight instanceof DoubleValue) ||
                              (primLeft instanceof DoubleValue)) {
            double rr = primRight.doubleValue();
            double ll = primLeft.doubleValue();
            double res;
            if (op.equals("+")) {
                res = rr + ll;
            } else if (op.equals("-")) {
                res = rr - ll;
            } else if (op.equals("*")) {
                res = rr * ll;
            } else if (op.equals("/")) {
                res = rr / ll;
            } else {
                throw new ParseException("Unknown operation: " + op);
            }
            return make(vm, res);
        }
        if ((primRight instanceof FloatValue) ||
                              (primLeft instanceof FloatValue)) {
            float rr = primRight.floatValue();
            float ll = primLeft.floatValue();
            float res;
            if (op.equals("+")) {
                res = rr + ll;
            } else if (op.equals("-")) {
                res = rr - ll;
            } else if (op.equals("*")) {
                res = rr * ll;
            } else if (op.equals("/")) {
                res = rr / ll;
            } else {
                throw new ParseException("Unknown operation: " + op);
            }
            return make(vm, res);
        }
        if ((primRight instanceof LongValue) ||
                              (primLeft instanceof LongValue)) {
            long rr = primRight.longValue();
            long ll = primLeft.longValue();
            long res;
            if (op.equals("+")) {
                res = rr + ll;
            } else if (op.equals("-")) {
                res = rr - ll;
            } else if (op.equals("*")) {
                res = rr * ll;
            } else if (op.equals("/")) {
                res = rr / ll;
            } else {
                throw new ParseException("Unknown operation: " + op);
            }
            return make(vm, res);
        } else {
            int rr = primRight.intValue();
            int ll = primLeft.intValue();
            int res;
            if (op.equals("+")) {
                res = rr + ll;
            } else if (op.equals("-")) {
                res = rr - ll;
            } else if (op.equals("*")) {
                res = rr * ll;
            } else if (op.equals("/")) {
                res = rr / ll;
            } else {
                throw new ParseException("Unknown operation: " + op);
            }
            return make(vm, res);
        }
    }   
}
