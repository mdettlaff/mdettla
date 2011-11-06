cp Test.java.backup Test.java
./import_static.sh org.mockito.Mockito.verify Test.java
diff Test.java.ok Test.java | colordiff
