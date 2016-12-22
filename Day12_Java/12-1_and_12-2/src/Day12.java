import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Vector;

public class Day12 {
	private static Map<Character, Integer> registers = new HashMap<Character, Integer>();
	private static Vector<String> commands = new Vector<String>();
	
	private static boolean isNumeric(String s) {  
	    return s.matches("[-+]?\\d*\\.?\\d*");
	}
	
	private static int handleCommand(int i) {
		String line = commands.get(i);
		String[] commandLine = line.split(" +");
		String command = commandLine[0];
		String reg = commandLine[1];

		if (command.equals("cpy")) {
			int n = regOrValue(reg);;
			char toReg = commandLine[2].charAt(0);
			registers.put(toReg, n);
		}
		else if (command.equals("inc")) {
			char regc = reg.charAt(0);
			registers.put(regc, registers.getOrDefault(regc, 0) + 1);
		}
		else if (command.equals("dec")) {
			char regc = reg.charAt(0);
			registers.put(regc, registers.getOrDefault(regc, 0) - 1);
		}
		else if (command.equals("jnz")) {
			int n = regOrValue(reg);
			return n == 0 ? i + 1 : i + Integer.parseInt(commandLine[2]);
		}
		
		return i + 1;
	}
	
	private static int regOrValue(String reg) {
		if (isNumeric(reg)) {
			return Integer.parseInt(reg);
		}
		else {
			return registers.getOrDefault(reg.charAt(0), 0);
		}
	}

	public static void main(String[] args) {
		Path path = Paths.get("../input.txt");
		
		try {
			Files.lines(path).forEachOrdered(line -> commands.addElement(line));
		} catch (IOException e) {
			System.out.println(e);
		}
		
		int i = 0;
		while (i < commands.size()) {
			i = handleCommand(i);
		}
		System.out.println("Answer to part 1: " + registers.get('a'));

		// reset for part 2
		i = 0;
		registers.clear();
		registers.put('c', 1);
		while (i < commands.size()) {
			i = handleCommand(i);
		}
		System.out.println("Answer to part 2: " + registers.get('a'));
	}
}
