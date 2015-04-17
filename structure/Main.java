import java.io.*;
import java.util.*;
import java.math.*;
import java.text.*;
public class Main {
public static void main(String[] args) throws IOException {
	BufferedReader in;
	StringBuilder out = new StringBuilder();
	File file = new File("DataSets.txt");
	in = new BufferedReader(new FileReader(file));
	String line, lines[];
	ArrayList<String> names = new ArrayList<String>();
	while ((line = in.readLine()) != null) {
		lines = line.split("\\s+");
		for( int i = 0; i<  lines.length; i++ )
		{
			names.add(lines[i]);
		}
	}
	out.append("c(");
	for ( int i = 0; i < names.size(); i++ )
	{
		out.append("\'"+(names.get(i))+"\'");
		if ( i != names.size() - 1)
			out.append(",");
		
	}
	out.append(")");
	System.out.print(out);
}
}

