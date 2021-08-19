/*--------------------------------------------------------------------*/
/* centi.c                                                            */
/* Execution: ./centi [filename]                                      */
/* Dependencies: none                                                 */
/*--------------------------------------------------------------------*/

/*-includes-----------------------------------------------------------*/

/* Feature test macros. */

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

/*-defines------------------------------------------------------------*/

#define CENTI_VERSION "0.0.1"
#define CENTI_TAB_STOP 4

/* Bitwise-AND with 00011111, which is (almost) what ctrl does in the
   terminal. (Really clears bits 5, 6.) */

#define CTRL_KEY(k) ((k) & 0x1f)

/* Assign large ints to arrow keys st they do not conflict with regular
   char input. */

enum editorKey {
  BACKSPACE = 127,
  ARROW_LEFT = 1000,
  ARROW_RIGHT,
  ARROW_UP,
  ARROW_DOWN,
  DEL_KEY,
  HOME_KEY,
  END_KEY,
  PAGE_UP,
  PAGE_DOWN
};

/* Assign ints to coding "parts of speech." */

enum editorHighlight {
  HL_NORMAL = 0,
  HL_COMMENT,
  HL_MLCOMMENT,
  HL_KEYWORD1,
  HL_KEYWORD2,
  HL_STRING,
  HL_NUMBER,
  HL_MATCH
};

/* TODO */

#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)

/*-data---------------------------------------------------------------*/

/* Stroes syntax hl information for different filetypes. */

struct editorSyntax {
  char* filetype; // name of filetype
  char** filematch; // array of strings to identify file by
  char** keywords; // array of keywords
  char* singleline_comment_start;
  char* multiline_comment_start;
  char* multiline_comment_end;
  int flags; // highlight numbers or strings?
};

/* Stores a row of text. */

typedef struct erow {
  int idx; // row's index in file
  int size;
  int rsize;
  char* chars;
  char* render; // chars to show user
  // identifies each char with a highlighting group (i.e. comment,
  // string, etc.)
  unsigned char* hl; // unsigned, i.e. [0, 255]
  int hl_open_comment; 
} erow;

/* Stores the editor state. */

struct editorConfig {
  int cx, cy; // cursor position
  int rx; // cursor position in render
  int col_off; // column offset
  int row_off; // row offset
  int screen_rows;
  int screen_cols;
  int num_rows;
  erow* row;
  int dirty; // has the file been modified since the last write?
  char* filename;
  char status_msg[80];
  time_t status_msg_time;
  struct editorSyntax *syntax;
  struct termios og_termios;
};

struct editorConfig E;

/*-filetypes----------------------------------------------------------*/

/* Possible file extensions for C filetypes. */

char* C_HL_extensions[] = { ".c", ".h", ".cpp", NULL };

/* */

char* C_HL_keywords[] = {
  "switch", "if", "while", "for", "break", "continue", "return", "else",
  "struct", "union", "typedef", "static", "enum", "class", "case",

  // secondary keywords get a pipe appended
  "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
  "void|", NULL
};

/* Database of editorSyntax's for filetype highlighting. */

struct editorSyntax HLDB[] = {
  { "C",
    C_HL_extensions,
    C_HL_keywords,
    "//", "/*", "*/",
    HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS },
};

/* Length of the HLDB array. */

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))


/*-prototypes---------------------------------------------------------*/

void editorSetStatusMessage(const char* fmt, ...);
void editorRefreshScreen();
char* editorPrompt(char* prompt, void (*callback)(char*, int));

/*-syntax-hl---------------------------------------------------------*/

/* Return true if c is a separator char, false otherwise. */

int isSeparator(int c) {
  return isspace(c) || c == '\0' ||
    strchr(",.()+-/*=~%<>[];", c) != NULL;
}

/* Parses row, setting each char's syntax highlighting value. */

void editorUpdateSyntax(erow* row) {
  // reallocate, in case row is new/has been modified
  row->hl = realloc(row->hl, row->rsize);
  // set all chars to HL_NUMBER
  memset(row->hl, HL_NORMAL, row->rsize);

  if (E.syntax == NULL) return;

  char** keywords = E.syntax->keywords;
  
  char* scs = E.syntax->singleline_comment_start; // alias
  char* mcs = E.syntax->multiline_comment_start;
  char* mce = E.syntax->multiline_comment_end;
  
  int scs_len = scs ? strlen(scs) : 0;
  int mcs_len = mcs ? strlen(mcs) : 0;
  int mce_len = mce ? strlen(mce) : 0;

  int prev_sep = 1;
  int in_string = 0;
  // check if previous row was in a ML comment
  int in_comment = (row->idx > 0 && E.row[row->idx - 1].hl_open_comment);

  int i = 0;
  while (i < row->rsize) {
    char c = row->render[i];
    unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;

    // if SL comment hl is on and not in a string or ML comment
    if (scs_len && !in_string && !in_comment) {
      // check if comment syntax matches
      if (!strncmp(&row->render[i], scs, scs_len)) {
        // highlight the entire line
        memset(&row->hl[i], HL_COMMENT, row->rsize - i);
        break;
      }
    }

    // if not in a string and mcs, mce well-defined
    if (mcs_len && mce_len && !in_string) {
      if (in_comment) {
        // if in comment, just hl the current char
        row->hl[i] = HL_MLCOMMENT;
        // check if we're at the end of the comment
        if (!strncmp(&row->render[i], mce, mce_len)) {
          memset(&row->hl[i], HL_MLCOMMENT, mce_len);
          i += mce_len;
          in_comment = 0;
          prev_sep = 1;
          continue;
        } else { // otherwise continue
          i++;
          continue;
        }
      } else if (!strncmp(&row->render[i], mcs, mcs_len)) {
        memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
        i += mcs_len;
        in_comment = 1;
        continue;
      }
    }

    if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
      if (in_string) {
        row->hl[i] = HL_STRING;
        // if backslash precedes at least one more char, consider as
        // escape sequence
        if (c == '\\' && i + 1 < row->rsize) {
          row->hl[i + 1] = HL_STRING;
          i += 2;
          continue;
        }
        // if closing quote
        if (c == in_string) in_string = 0;
        i++;
        prev_sep = 1; // consider quote as separator
        continue;
      } else { // check if we're in a string
        if (c == '"' || c == '\'') {
          in_string = c; 
          row->hl[i] = HL_STRING;
          i++;
          continue;
        }
      }
    }
    
    if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
      // if previous char is separator or a highlighted digit
      if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
          (c == '.' && prev_hl == HL_NUMBER)) {
        row->hl[i] = HL_NUMBER;
        i++;
        prev_sep = 0;
        continue;
      }
    }

    if (prev_sep) {
      int j;
      for (j = 0; keywords[j]; j++) {
        int klen = strlen(keywords[j]);
        int kw2 = keywords[j][klen - 1] == '|';
        // if secondary keyword, decrement length for '|'
        if (kw2) klen--;

        // check if current word is a keyword followed by separator
        if (!strncmp(&row->render[i], keywords[j], klen) &&
            isSeparator(row->render[i + klen])) {
          // highlight keyword
          memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
          i += klen;
          break; // break since inner loop
        }
      }
      // continue if we broke out of inner loop
      if (keywords[j] != NULL) {
        prev_sep = 0;
        continue;
      }
    }
    prev_sep = isSeparator(c);
    i++;
  }

  int changed = (row->hl_open_comment != in_comment);
  // after processing the row, are we in a comment or not?
  row->hl_open_comment = in_comment; 
  if (changed && row->idx + 1 < E.num_rows) {
    // only update highlighting if the current row's hl_open_comment
    // changed, otherwise there will be no change in row idx + 1
    editorUpdateSyntax(&E.row[row->idx + 1]);
  }
}

/* Takes enumerated syntax group and returns ANSI color code. */

int editorSyntaxToColor(int hl) {
  switch (hl) {
    case HL_COMMENT: 
    case HL_MLCOMMENT: return 36;
    case HL_KEYWORD1: return 33;
    case HL_KEYWORD2: return 32;
    case HL_STRING: return 35;
    case HL_NUMBER: return 31;
    case HL_MATCH: return 34;
    default: return 37;
  }
}

/* Search HLDB[] database for a filetype that matches our current
   file. */

void editorSelectSyntaxHighlight() {
  E.syntax = NULL;
  if (E.filename == NULL) return;

  // ptr to beginning of filetype extension
  char* ext = strchr(E.filename, '.');

  for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
    struct editorSyntax* s = &HLDB[j];
    unsigned int i = 0;
    // loop through HLDB entry
    while (s->filematch[i]) {
      int is_ext = (s->filematch[i][0] == '.'); // is pattern an ext?
      // if pattern is an ext and it matches ext OR pattern matches
      // anywhere in the filename
      if ((is_ext && ext && !strcmp(ext, s->filematch[i])) ||
          (!is_ext && strstr(E.filename, s->filematch[i]))) {
        E.syntax = s;

        // rehighlight entire file
        int filerow;
        for (filerow = 0; filerow < E.num_rows; filerow++)
          editorUpdateSyntax(&E.row[filerow]);
        
        return;
      }
      i++;
    }
  }
}

/*-terminal-----------------------------------------------------------*/

/* Error-handing function */

void die(const char* s) {
  write(STDOUT_FILENO, "\x1b[2J", 4);
  write(STDOUT_FILENO, "\x1b[H", 3);
  
  perror(s); // print errno (global)
  exit(1); // exit with failure
}

/* Returns terminal settings to original state. */

void disableRawMode() {
  // TCSAFLUSH discards unread input prior to applying config
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.og_termios) == -1)
    die("tcsetattr");
}

/* Saves the current terminal settings, then enables raw mode on the
   terminal. */

void enableRawMode() {
  // read terminal's attributes into og_termios
  if (tcgetattr(STDIN_FILENO, &E.og_termios) == -1)
    die("tcgetattr");
  atexit(disableRawMode);

  struct termios raw = E.og_termios; 
  // turn off local flags
  // disable echo, canonical mode, c-V, interrupt
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  // turn off input flags
  // disable carriage return, start/stop, some others
  raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  // turn off ouput flags
  raw.c_oflag &= ~(OPOST);
  // set character size to 8 bits per byte
  raw.c_cflag |= (CS8);
  // set control characters
  raw.c_cc[VMIN] = 0; // read() returns on any input
  raw.c_cc[VTIME] = 1; // read() returns after 100ms

  // apply modified attributes to terminal
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1)
    die("tcsetattr");
}

/* Wait for one keypress, then return it. */

int editorReadKey() {
  int nread;
  char c;
  while ((nread = read(STDIN_FILENO, &c, 1)) != 1)
    if (nread == -1 && errno != EAGAIN) die("read");

  // handle escape characters
  if (c == '\x1b') {
    char seq[3];
    // read two more bytes into seq
    if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
    if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';

    if (seq[0] == '[') {
      if (seq[1] >= '0' && seq[1] <= '9') {
        if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
        if (seq[2] == '~') {
          switch (seq[1]) {
            case '1': return HOME_KEY;
            case '3': return DEL_KEY;
            case '4': return END_KEY;
            case '5': return PAGE_UP;
            case '6': return PAGE_DOWN;
            case '7': return HOME_KEY;
            case '8': return END_KEY;
          }
        }
      // handle arrow key sequences
      } else {
        switch (seq[1]) {
          case 'A': return ARROW_UP;
          case 'B': return ARROW_DOWN;
          case 'C': return ARROW_RIGHT;
          case 'D': return ARROW_LEFT;
          case 'H': return HOME_KEY;
          case 'F': return END_KEY;
        }
      }
    } else if (seq[0] == 'O') {
      switch (seq[1]) {
        case 'H': return HOME_KEY;
        case 'F': return END_KEY;
      }
    }
    return '\x1b';
  } else
    return c;
}

/* Writes the position of the cursor at the int* arguments. Returns
   0 on success, -1 on failure. */

int getCursorPosition(int* rows, int* cols) {
  char buf[32];
  unsigned int i = 0;

  // query terminal for cursor position
  if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) return -1;

  while (i < sizeof(buf) - 1) {
    if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
    if (buf[i] == 'R') break;
    i++;
  }
  buf[i] = '\0';

  if (buf[0] != '\x1b' || buf[1] != '[') return -1;
  // parse rows:cols
  if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;

  return 0;
}

/* Places the window size of the terminal at the int* arguments. 
   Returns 0 on success, -1 on failure. */

int getWindowSize(int* rows, int* cols) {
  struct winsize ws;

  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
    // fallback: move cursor to bottom right corner
    if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
    return getCursorPosition(rows, cols);
  }
  else {
    *cols = ws.ws_col;
    *rows = ws.ws_row;
    return 0;
  }
}

/*-row-ops------------------------------------------------------------*/

/* Compute the position of the cursor in the render from its position
   in erow. */

int editorRowCxToRx(erow* row, int cx) {
  int rx = 0;
  int j;
  for (j = 0; j < cx; j++) {
    if (row->chars[j] == '\t')
      rx += (CENTI_TAB_STOP - 1) - (rx % CENTI_TAB_STOP);
    rx++;
    // sets rx = rx + CENTI_TAB_STOP - (rx % CENTI_TAB_STOP) if '\t'
  }
  return rx;
}



/* Compute the position of the cursor in chars from its position in 
   the render. */

int editorRowRxToCx(erow* row, int rx) {
  int cur_rx = 0;
  int cx;
  for (cx = 0; cx < row->size; cx++) {
    if (row->chars[cx] == '\t') // if a tab
      cur_rx += (CENTI_TAB_STOP - 1) - (cur_rx % CENTI_TAB_STOP);
    cur_rx++;

    if (cur_rx > rx) return cx;
  }
  return cx;
}

/* Fill render with the chars content of erow. */

void editorUpdateRow(erow* row) {
  int tabs = 0; // number of tabs in the string
  int j;
  for (j = 0; j < row->size; j++)
    if (row->chars[j] == '\t') tabs++;
  
  free(row->render);
  // malloc enough memory for 8 space tabs
  row->render = malloc(row->size + tabs*(CENTI_TAB_STOP - 1) + 1);

  int idx = 0;
  for (int j = 0; j < row->size; j++) {
    if (row->chars[j] == '\t') {
      row->render[idx++] = ' '; // advance cursor one column
      // append spaces until we reach a tab stop, i.e. a column
      // divisible by 8
      while (idx % CENTI_TAB_STOP != 0) row->render[idx++] = ' ';
    } else
      row->render[idx++] = row->chars[j];
  }
  row->render[idx] = '\0';
  row->rsize = idx;

  editorUpdateSyntax(row);
}

/* Insert row s to the editor. */

void editorInsertRow(int at, char* s, size_t len) {
  if (at < 0 || at > E.num_rows) return;
  
  // make space for a new erow in row[]
  E.row = realloc(E.row, sizeof(erow) * (E.num_rows + 1));
  memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.num_rows - at));
  for (int j = at + 1; j <= E.num_rows; j++)
    E.row[j].idx++; // increment idx of following rows

  E.row[at].idx = at;

  E.row[at].size = len;
  E.row[at].chars = malloc(len + 1);
  memcpy(E.row[at].chars, s, len);
  E.row[at].chars[len] = '\0';

  E.row[at].rsize = 0;
  E.row[at].render = NULL;
  E.row[at].hl = NULL;
  E.row[at].hl_open_comment = 0;
  editorUpdateRow(&E.row[at]);
  
  E.num_rows++;
  E.dirty++;
}

/* Free memory allocated to erow row. */

void editorFreeRow(erow* row) {
  free(row->render);
  free(row->chars);
  free(row->hl);
}

/* Delete the row with index at. */

void editorDelRow(int at) {
  if (at < 0 || at >= E.num_rows) return;
  editorFreeRow(&E.row[at]);
  memmove(&E.row[at], &E.row[at + 1], sizeof(erow) +
         (E.num_rows - at - 1));
  for (int j = at; j < E.num_rows - 1; j++)
    E.row[j].idx--; // decrement idx of following rows
  E.num_rows--;
  E.dirty++;
}

/* Insert char c in row at given position at. */

void editorRowInsertChar(erow* row, int at, int c) {
  if (at < 0 || at > row->size) at = row->size; // validate at
  // row->size + 1 for c, + 1 for '\0'
  row->chars = realloc(row->chars, row->size + 2);
  // memcpy, but safe for overlapping arrays
  memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
  row->size++;
  row->chars[at] = c;
  editorUpdateRow(row);
  E.dirty++;
}

/* Append string s of length len to erow row. */

void editorRowAppendString(erow* row, char* s, size_t len) {
  row->chars = realloc(row->chars, row->size + len + 1);
  memcpy(&row->chars[row->size], s, len);
  row->size += len;
  row->chars[row->size] = '\0';
  editorUpdateRow(row);
  E.dirty++;
}

/* Delete char c in row at given position at. */

void editorRowDelChar(erow* row, int at) {
  if (at < 0 || at >= row->size) return;
  memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
  row->size--;
  editorUpdateRow(row);
  E.dirty++;
}

/*-editor-ops---------------------------------------------------------*/

/* Insert char c at the cursor position. */

void editorInsertChar(int c) {
  // add a row if EOF
  if (E.cy == E.num_rows) editorInsertRow(E.num_rows, "", 0);
  editorRowInsertChar(&E.row[E.cy], E.cx, c);
  E.cx++;
}


/* Creates a new line at the current cursor position, splitting text 
   across lines as necessary. */

void editorInsertNewline() {
  // if at the start of a line, just insert an empty line
  if (E.cx == 0) editorInsertRow(E.cy, "", 0);
  else {
    erow* row = &E.row[E.cy];
    // create a new line, splitting the old line at cx
    editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
    // reassign the row ptr, since editorInsertRow() calls realloc()
    // (which may invalidate the pointer)
    row = &E.row[E.cy];
    row->size = E.cx;
    // truncate the row, and update the render for the truncated row
    row->chars[row->size] = '\0';
    editorUpdateRow(row);
  }
  E.cy++;
  E.cx = 0;
}

/* Delete char c that is left of the cursor. */

void editorDelChar() {
  if (E.cy == E.num_rows) return;
  if (E.cx == 0 && E.cy == 0) return; // nothing to delete

  erow* row = &E.row[E.cy];
  if (E.cx > 0) {
    editorRowDelChar(row, E.cx - 1);
    E.cx--;
  } else {
    E.cx = E.row[E.cy - 1].size; // move cusor back to end of prev line
    editorRowAppendString(&E.row[E.cy - 1], row->chars, row->size);
    editorDelRow(E.cy);
    E.cy--;
  }
}

/*-file-IO------------------------------------------------------------*/


/* toString method for erow structs. */

char* editorRowsToString(int* buflen) {
  int totlen = 0;
  int j;
  for (j = 0; j < E.num_rows; j++) 
    totlen += E.row[j].size + 1; // add 1 for each newline char
  *buflen = totlen;

  char* buf = malloc(totlen);
  char* p = buf;
  for (j = 0; j < E.num_rows; j++) {
    memcpy(p, E.row[j].chars, E.row[j].size);
    p += E.row[j].size;
    *p = '\n';
    p++;
  }

  return buf;
}

/* Open a file in the editor. */

void editorOpen(char* filename) {
  free(E.filename);
  E.filename = strdup(filename); // copies string, allocating memory

  editorSelectSyntaxHighlight();
  
  FILE* fp = fopen(filename, "r");
  if (!fp) die("fopen");

  char* line = NULL;
  size_t line_cap = 0;
  ssize_t line_len;

  // read in lines from the file, appending to row[]
  while ((line_len = getline(&line, &line_cap, fp)) != -1) {
    while (line_len > 0 && (line[line_len - 1] == '\n' ||
                            line[line_len - 1] == '\r'))
      line_len--;
    editorInsertRow(E.num_rows, line, line_len);
  }
  free(line);
  fclose(fp);
  E.dirty = 0;
}

/* Write the contents of the editor to the open file. */

void editorSave() {
  if (E.filename == NULL) {
    E.filename = editorPrompt("save as: %s (esc to cancel)", NULL);
    if (E.filename == NULL) {
      editorSetStatusMessage("save aborted");
      return;
    }
    editorSelectSyntaxHighlight();
  }

  int len;
  char* buf = editorRowsToString(&len);

  // create a new file if it doesn't exist with read/write
  // permissions for owner
  int fd = open(E.filename, O_RDWR | O_CREAT, 0644);

  if (fd != -1) { 
    if (ftruncate(fd, len) != -1) { // truncate file to length of buf
      // overwrite the file with contents of buf
      if (write(fd, buf, len) == len) {
        close(fd);
        free(buf);
        E.dirty = 0;
        editorSetStatusMessage("%d bytes written to disk", len);
        return;
      }
    }
    close(fd);
  }
  free(buf);
  editorSetStatusMessage("I/O error during write: %s", strerror(errno));
}


/*-find---------------------------------------------------------------*/

/* Callback fct to implement incremental search. Exits search if user
   inputs ret or esc. Searches for query + c on any other input c. */

void editorFindCallback(char* query, int key) {
  // for forwards and backwards search
  static int last_match = -1;
  static int direction = 1;

  // unhighlight matched string on exit
  static int saved_hl_line;
  static char* saved_hl = NULL;
  if (saved_hl) {
    memcpy(E.row[saved_hl_line].hl, saved_hl,
           E.row[saved_hl_line].rsize);
    free(saved_hl);
    saved_hl = NULL;
  }
  
  // return if user is exiting search mode
  if (key == '\r' || key == '\x1b') {
    last_match = -1;
    direction = 1;
    return;
  } else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
    direction = 1;
  } else if (key == ARROW_LEFT || key == ARROW_UP) {
    direction = -1;
  } else {
    last_match = -1;
    direction = 1;
  }

  // otherwise, search for the updated query
  if (last_match == -1) direction = 1;
  int current = last_match;
  int i;
  for (i = 0; i < E.num_rows; i++) {
    current += direction; // start near last search
    // wrap-around search
    if (current == -1) current = E.num_rows - 1;
    else if (current == E.num_rows) current = 0;
    
    erow* row = &E.row[current];
    // search for a match (ignores case)
    char* match = strcasestr(row->render, query);
    if (match) {
      last_match = current;
      E.cy = current;
      // locate position in the render
      E.cx = editorRowRxToCx(row, match - row->render);
      // set row_off to the bottom of the file
      // this way, search cursor will end up at the top of the screen
      // on call editorScroll()
      E.row_off = E.num_rows;

      saved_hl_line = current;
      saved_hl = malloc(row->rsize);
      memcpy(saved_hl, row->hl, row->rsize);
      // set highlighting for the matched string
      memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
      break;
    }
    // TODO failure message if no match
  }
}

/* Search for user-inputted string, and move cursor to its location. */

void editorFind() {
  // save cursor position prior to search
  int og_cx = E.cx;
  int og_cy = E.cy;
  int og_col_off = E.col_off;
  int og_row_off = E.row_off;
  
  char* query = editorPrompt("search: %s (esc to cancel)",
                             editorFindCallback);
  if (query) {
    free(query);
  } else {
    E.cx = og_cx;
    E.cy = og_cy;
    E.col_off = og_col_off;
    E.row_off = og_row_off;
  }
}

/*-append-buffer------------------------------------------------------*/

/* Dynamic string type that supports appending. */

struct abuf {
  char* b;
  int len;
};

#define ABUF_INIT {NULL, 0}

/* Appends string s to abuf ab. */

void abAppend(struct abuf* ab, const char* s, int len) {
  // get a block of memory of current string size + len
  char* new = realloc(ab->b, ab->len + len);
  if (new == NULL) return;
  
  memcpy(&new[ab->len], s, len);
  ab->b = new;
  ab->len += len;
}

/* Free dynamic memory used by abuf ab. */

void abFree(struct abuf* ab) {
  free(ab->b);
}

/*-input--------------------------------------------------------------*/


/* Display prompt to the user, and return user input. */

char* editorPrompt(char* prompt, void (*callback)(char*, int)) {
  size_t bufsize = 128;
  char* buf = malloc(bufsize);

  size_t buflen = 0;
  buf[0] = '\0';

  while(1) {
    editorSetStatusMessage(prompt, buf);
    editorRefreshScreen();

    int c = editorReadKey();
    if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
      if (buflen != 0) buf[--buflen] = '\0';
    } else if (c == '\x1b') {
      editorSetStatusMessage("");
      // check allows for NULL as callback (i.e. no callback used)
      if (callback) callback(buf, c);
      free(buf);
      return NULL;
    } else if (c == '\r') { // enter, user ends input
      if (buflen != 0) {
        // clear status message and return input
        editorSetStatusMessage("");
        if (callback) callback(buf, c);
        return buf;
      }
      // verify that c isn't a special key
    } else if (!iscntrl(c) && c < 128) {
      if (buflen == bufsize - 1) { // resize if necessary
        bufsize *= 2;
        buf = realloc(buf, bufsize);
      }
      // append c to buf
      buf[buflen++] = c;
      buf[buflen] = '\0';
    }
    if (callback) callback(buf, c);
  }
}

/* Move the cursor according to user input. */

void editorMoveCursor(int key) {
  // is the cursor on an actual line?
  erow* row = (E.cy >= E.num_rows) ? NULL : &E.row[E.cy];
  
  switch (key) {
    case ARROW_LEFT:
    case CTRL_KEY('b'):
      if (E.cx != 0) E.cx--;
      else if (E.cy > 0) {
        E.cy--;
        E.cx = E.row[E.cy].size;
      }
      break;
    case ARROW_RIGHT:
    case CTRL_KEY('f'):
      if (row && E.cx < row->size) // is cursor within a line?
        E.cx++;
      else if (row && E.cx == row->size) {
        E.cy++;
        E.cx = 0;
      }
      break;
    case ARROW_UP:
    case CTRL_KEY('p'):
      if (E.cy != 0) E.cy--;
      break;
    case ARROW_DOWN:
    case CTRL_KEY('n'):
      if (E.cy < E.num_rows) E.cy++;
      break;
  }

  // make cursor snap to end of line
  row = (E.cy >= E.num_rows) ? NULL : &E.row[E.cy];
  int row_len = row ? row->size : 0;
  if (E.cx > row_len) E.cx = row_len; // snap if past end of line
}

/* Wait for one keypress, then handle it. */

void editorProcessKeypress() {
  static int dirty_quit = 0;
 
  int c = editorReadKey();
  switch (c) {
    case '\r':
      editorInsertNewline();
      break;
    
    case CTRL_KEY('q'):
      if (E.dirty) {
        editorSetStatusMessage("file has unsaved changes. quit anyways? y/n");
        dirty_quit = 1;
        return;
      }
      write(STDOUT_FILENO, "\x1b[2J", 4);
      write(STDOUT_FILENO, "\x1b[H", 3);
      exit(0);
      break;

    case CTRL_KEY('s'):
      editorSave();
      break;

    case HOME_KEY:
      E.cx = 0;
      break;

    case END_KEY:
      if (E.cy < E.num_rows)
        E.cx = E.row[E.cy].size;
      break;

    case CTRL_KEY('f'):
      editorFind();
      break;

    case BACKSPACE:
    case CTRL_KEY('h'):
    case DEL_KEY:
      if (c == DEL_KEY) editorMoveCursor(ARROW_RIGHT);
      editorDelChar();
      break;

    case PAGE_UP:
    case PAGE_DOWN:
      {
        if (c == PAGE_UP)
          E.cy = E.row_off;
        else if (c == PAGE_DOWN) {
          E.cy = E.row_off + E.screen_rows - 1;
          if (E.cy > E.num_rows) E.cy = E.num_rows;
        }
        
        int times = E.screen_rows;
        while (times--)
          editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
      }
      break;

    case ARROW_UP:
    case CTRL_KEY('p'):
    case ARROW_LEFT:
    // case CTRL_KEY('f'):
      // TODO change to alt-f
    case ARROW_DOWN:
    case CTRL_KEY('n'):
    case ARROW_RIGHT:
    case CTRL_KEY('b'):
      editorMoveCursor(c);
      break;

    case CTRL_KEY('l'):
    case '\x1b':
      break;
      
    default:
      if (dirty_quit) {
        if (c == 'y') {
          write(STDOUT_FILENO, "\x1b[2J", 4);
          write(STDOUT_FILENO, "\x1b[H", 3);
          exit(0);
          break;
        }
        if (c == 'n') {
          editorSetStatusMessage("");
          break;
        }
      } 
      editorInsertChar(c);
      break;
  }
}

/*-output-------------------------------------------------------------*/

/* If the cursor is not visible, update the row offset. */

void editorScroll() {
  E.rx = 0;
  if (E.cy < E.num_rows)
    E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);
  
  if (E.cy < E.row_off) // cursor above visible window
    E.row_off = E.cy;
  if (E.cy >= E.row_off + E.screen_rows) // cursor below visible window
    E.row_off = E.cy - E.screen_rows + 1;
  if (E.rx < E.col_off) // cursor left of visible window
    E.col_off = E.rx;
  if (E.rx >= E.col_off + E.screen_cols) // cursor right of window
    E.col_off = E.rx - E.screen_cols + 1;
}

/* Draws a left border on the screen. */

void editorDrawRows(struct abuf* ab) {
  int y;
  for (y = 0; y < E.screen_rows; y++) {
    int filerow = y + E.row_off;
    // draw row outside of the text buffer
    if (filerow >= E.num_rows) { 
      // print welcome message if no file has been opened
      if (E.num_rows == 0 && y == E.screen_rows / 3) {
        char welcome[80];
        int welcome_len = snprintf(welcome, sizeof(welcome),
                                   "centi editor -- version %s", CENTI_VERSION);
        if (welcome_len > E.screen_cols) welcome_len = E.screen_cols;
        // center welcome message
        int padding = (E.screen_cols - welcome_len) / 2;
        if (padding) {
          abAppend(ab, "~", 1);
          padding--;
        }
        while (padding--) abAppend(ab, " ", 1);
        abAppend(ab, welcome, welcome_len);
      }
      // line without welcome message
      else abAppend(ab, "~", 1);
    }
    // draw row part of the text buffer
    else { 
      int len = E.row[filerow].rsize - E.col_off;
      if (len < 0) len = 0;
      // truncate line if necessary
      if (len > E.screen_cols) len = E.screen_cols;

      char* c = &E.row[filerow].render[E.col_off];
      unsigned char* hl = &E.row[filerow].hl[E.col_off];
      int current_color = -1;
      int j;
      // syntax highlighting
      for (j = 0; j < len; j++) {
        // nonprintable chars
        if (iscntrl(c[j])) {
          char sym = (c[j] <= 26) ? '@' + c[j] : '?';
          abAppend(ab, "\x1b[7m", 4); // print with inverted colors
          abAppend(ab, &sym, 1);
          abAppend(ab, "\x1b[m", 3);
          if (current_color != -1) { // reset coloring
            char buf[16];
            int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
            abAppend(ab, buf, clen);
          }
        } else if (hl[j] == HL_NORMAL) { // color white if normal
          if (current_color != -1) {
            abAppend(ab, "\x1b[39m", 5);
            current_color = -1;
          }
          abAppend(ab, &c[j], 1);
        } else { // otherwise, look up color
          int color = editorSyntaxToColor(hl[j]);
          if (color != current_color) {
            current_color = color;
            char buf[16];
            // write escape sequence to buf
            int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
            abAppend(ab, buf, clen);
          }
          // write char itself
          abAppend(ab, &c[j], 1);
        }
      }
      abAppend(ab, "\x1b[39m", 5);
    }
    
    // erase line to the right of cursor
    abAppend(ab, "\x1b[K", 3);
    abAppend(ab, "\r\n", 2); // status bar
  }
}

/* Draw the status bar. */

void editorDrawStatusBar(struct abuf* ab) {
  abAppend(ab, "\x1b[7m", 4); // invert colors
  char status[80], rstatus[80];
  // snprintf is nice to handle strings of a priori unknown length
  // print filename and number of lines
  int len = snprintf(status, sizeof(status), "%.20s - %d lines %s",
                     E.filename ? E.filename : "no file open",
                     E.num_rows, E.dirty ? "(modified)" : "");
  // print current line and percentage
  float pct = (float) (E.cy + 1) / E.num_rows * 100;
  pct = (E.num_rows) ? pct : 0;
  int rlen = snprintf(rstatus, sizeof(rstatus), "%s | %d/%d %.0f%%",
                      E.syntax ? E.syntax->filetype : "no ft",
                      E.cy + 1, E.num_rows, pct);
  if (len > E.screen_cols) len = E.screen_cols;
  abAppend(ab, status, len);
  while (len < E.screen_cols) {
    if (E.screen_cols - len == rlen) {
      abAppend(ab, rstatus, rlen);
      break;
    } else {
      abAppend(ab, " ", 1);
      len++;
    }
  }
  abAppend(ab, "\x1b[m", 3); // invert colors
  abAppend(ab, "\r\n", 2); // row for status message
}

/* Draw the message bar below the status bar. */

void editorDrawMessageBar(struct abuf* ab) {
  abAppend(ab, "\x1b[K", 3); // clear message
  int msglen = strlen(E.status_msg);
  if (msglen > E.screen_cols) msglen = E.screen_cols;
  // print message if < 5s old
  if (msglen && time(NULL) - E.status_msg_time < 5)
    abAppend(ab, E.status_msg, msglen);
}

/* Clears screen and positions cursor at top left corner. */


void editorRefreshScreen() {
  editorScroll();
  
  struct abuf ab = ABUF_INIT;

  // hide cursor while we work our magic
  abAppend(&ab, "\x1b[?25l", 6);
  // position cursor at (1, 1)
  abAppend(&ab, "\x1b[H", 3);
  
  editorDrawRows(&ab);
  editorDrawStatusBar(&ab);
  editorDrawMessageBar(&ab);
  
  // get cursor position 
  char buf[32];
  // add 1 since terminal uses 1-indexed values
  snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.row_off) + 1,
                                            (E.rx - E.col_off) + 1);
  // reposition cursor
  abAppend(&ab, buf, strlen(buf));

  // show cursor
  abAppend(&ab, "\x1b[?25h", 6);

  // write abuf in one go
  write(STDOUT_FILENO, ab.b, ab.len);
  abFree(&ab);
}

/* Set the status message to be displayed in the status bar. */

void editorSetStatusMessage(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  // allows us to write any number of arguments
  vsnprintf(E.status_msg, sizeof(E.status_msg), fmt, ap);
  va_end(ap);
  E.status_msg_time = time(NULL);
}

/*-init---------------------------------------------------------------*/

/* Initializes editor state. */

void initEditor() {
  E.cx = 0;
  E.cy = 0;
  E.rx = 0;
  E.col_off = 0;
  E.row_off = 0;
  E.num_rows = 0;
  E.row = NULL;
  E.dirty = 0;
  E.filename = NULL;
  E.status_msg[0] = '\0';
  E.status_msg_time = 0;
  E.syntax = NULL;
  
  if (getWindowSize(&E.screen_rows, &E.screen_cols) == -1)
    die("getWindowSize");
  E.screen_rows -= 2; // compensate for status bar
}

int main(int argc, char* argv[]) {
  enableRawMode();
  initEditor();
  if (argc >= 2) editorOpen(argv[1]);

  editorSetStatusMessage("ctrl-q to quit");

  while(1) {
    editorRefreshScreen();
    editorProcessKeypress();
  }
}
