import markdown as md
import sys

if __name__ == "__main__":
    args = sys.argv[1:]
    file_in, file_out = args[0], args[1]

md.markdownFromFile(input=file_in, output=file_out, encoding='utf-8')
