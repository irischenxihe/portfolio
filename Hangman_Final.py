"""
Hangman using a two classes
1 for logic and 1 for GUI
Widgets:root window,labels,Buttons, canvas
"""


import tkinter as tk
import random

#utility functions

#used to see if all chars in a word have been typed
#Thus you WIN when string_minus returns the empty string
def string_minus(s1,s2):
    """
    s1,s2:strings
    return:string consisting of letters in s1 not in s2
    string_minus("automobile","aom") ==> "utbile"
    string_minus("hola","lhaoxy") ==>""
    """
    diff=""
    for char in s1:
        if not(char in s2):
            diff=diff+char
    return diff


def make_display_word(word,letters_used):
    """
    word:str
    letters_used:str
    return:str - word, with underscores for letters
    that have not been used yet
    make_display_word ("automobile","aom") ==> "a __ __ o m o __ __ __ __"
    """
    display_word = ""
    for char in word:
        if char in letters_used:
            display_word += char+"  "
        else:
            display_word += "__  "
    return display_word





#logic part of game
#there should be NO GRAPHICS here.
class Hangman_game:

    def __init__(self):
        #self.words = ["locomotive","happiness","bayou","hangman"]
        #you can replace this with a list of the words in the file
        #words.txt (100,000 words!) on the Moodle page.
        #then you can uncomment the following:
        
        vocab = open("words.txt")
        self.words = list(vocab)
        """
        The file words.txt should be in the 
        same folder as this program for everything to work
        Download the file from
        """
        self.start()

    def start(self):
        self.game_over = False
        self.body_parts_remaining = ['tank','torso','leg1','leg2','arm1',\
                                     'arm2','headandlose','blood']
        self.body_parts_used =  []  
        self.word = random.choice(self.words).strip();
        print("Don't look at this hint "+str(self.word))
        self.letters_used = ""
        self.message_string="" # to report a win or loss to player
        

    def make_display(self):
        """
        creates word with underscores for missing letters
        uses make_display_word, defined at top
        make_display_word ("automobile","aom") ==> "a __ __ o m o __ __ __"
        """
        return make_display_word(self.word,self.letters_used)

    def update_game(self,char):
        """
        THIS IS THE MOST IMPORTANT METHOD
        char:str - a single letter
        result:update letters_used,
        If char not in word update body_parts_used and
        body_parts_remaining (using the update_body_parts method)  
        """

        #DO NOTHING if letter has already been typed
        #or if the game is over
        if char in self.letters_used or self.game_over:
            pass #leave this pass here
        else:
            self.letters_used += char
            self.update_message()
            if not (char in self.word) and (string_minus(self.word,self.letters_used) != ""):
                self.update_body_parts()
    
            if (string_minus(self.word,self.letters_used) == "") and \
               (self.body_parts_remaining != []):
                self.win_game()
            elif self.body_parts_remaining == []:
                self.lose_game()

            """
            update letters_used, also body parts information
            if necessary
            and check for the end of the game (win or loss)            
            
            """

    def update_message(self):
        new_string = self.letters_used[0]
        for letter in self.letters_used[1:]:
            new_string += ', '+letter
        self.message_string = 'letters used: '+new_string

        
    def update_body_parts(self):
        """
        add the next body part to body_parts_used and remove it from
        body_parts_remaining
        """
        if len(self.body_parts_remaining) > 0:
            self.body_parts_used += [self.body_parts_remaining[0]]
            self.body_parts_remaining.pop(0) 
    
    
    def win_game(self):
        """
        result:notify player of win (on label2)
        and update the game_over attribute
        """
        self.message_string = 'You Win'
        

    def lose_game(self):
        """
        result:notify player of loss
        and display the word they failed to guess
        (by updating self.message_string)
        and update the game_over attribute
        """
        self.message_string = 'You Lose. Keep trying or restart.'
        

#graphical layer
class Hangman_gui:
    """
    The graphical display part of
    Hangman
    """
    def __init__(self):
        """
        hangman_gui attributes:
        a root window, two labels, a button
        and a canvas
        label 1 will dislay the word
        label 2 displays "you won" or "you lost"
        """
        self.game = Hangman_game()
        
        self.tkroot = tk.Tk()     #The frame holding all the components
        self.tkroot.title("Hangman")
        self.tkroot.geometry("500x400+400+100")
        self.tkroot.focus()
       
        self.label1 = tk.Label(self.tkroot)
        self.label2 = tk.Label(self.tkroot)
        labelfont= ("Purisa",12,'bold')
        self.label2.config(bg = 'lightblue',font=labelfont)

        button = tk.Button(self.tkroot,text="Restart")
        button.config(command = self.restart)

        self.canvas = tk.Canvas(self.tkroot)
        self.make_canvas()
        
        self.label1.pack()
        self.label2.pack()
        button.pack()
       
        self.start()


    def start(self):
        self.tkroot.bind("<KeyPress>",self.onkeypress)
        #start the logic part, and then do the following:
        self.label1.config(text = make_display_word(self.game.word,""))
        self.label2.config(bg = 'lightblue', text = "take a guess")
        self.tkroot.mainloop()
        

    def make_canvas(self):
        """
        configure and pack canvas
        """
        self.canvas.config(height = 150,width=50)
        self.canvas.pack(expand=True,fill='both')


    def onkeypress(self,event):
        """
        define action carried out when user types a character
        it will be called self.onkeypress
        --grab the character typed by the user
        --update the logic game with the character typed by the user
        --update the texts on labels 1 and 2 
        display the word (by updating the text on label1)
        """
        ch = event.char  #get the character typed by the user
        self.game.update_game(ch)
        self.label1.config(text = self.game.make_display())
        self.label2.config(text = self.game.message_string)
        self.draw_stuff()


    def draw_wave(self,n):
        self.objects_x = {}
        self.objects_y = {}
        for i in range(0,n):
            x = random.randint(90,335)
            y = random.randint(120,270)
            si = str(i)
            name_x = "object_x_"+si
            name_y = "object_y_"+si
            self.objects_x[name_x] = self.canvas.create_arc(x,y, \
                    (x+45),(y+45), start=40, extent=108, style="arc", width=1)
            self.objects_y[name_y] = self.canvas.create_arc((x+35),(y-29), \
                    (x+80),(y+16), start=220, extent=108, style="arc", width=1)


    def delete_wave(self):
        for i in range(0,len(self.objects_x)):
            si = str(i)
            name_x = "object_x_"+si
            name_y = "object_y_"+si
            self.canvas.delete(self.objects_x[name_x])
            self.canvas.delete(self.objects_y[name_y])
        

    def draw_stuff(self):
        if len(self.game.body_parts_used) == 1:      #water tank
            self.canvas.create_line(80, 80, 80, 300, 420, 300, 420, 80, width=3)
            self.canvas.create_rectangle(82, 115, 419, 299, fill="lightblue", width=0)
            self.draw_wave(7)

        elif len(self.game.body_parts_used) == 7:       #head
            self.canvas.create_oval(137,74, 182,119, width=2)
            self.canvas.create_text(300,30, text = \
                                    "Help! It's your last chance to save me!", \
                                    anchor = "n", font=("Purisa",14,"bold"))          
            self.delete_wave()
            self.draw_wave(5)
            
        elif len(self.game.body_parts_used) == 2:       #neck and corso
            self.canvas.create_line(165,118, 171,125, 162,160, 230,215, width=2)
            self.canvas.create_line(175,113, 180,120, 214,123, 275,168, width=2)
            self.delete_wave()
            self.draw_wave(3)

        elif len(self.game.body_parts_used) == 5:        #upper arm
            self.canvas.create_line(214,123, 260,131, 298,128, 299,129, width=2)
            self.canvas.create_line(238,141, 262,145, 299,140, 300,139, width=2)
            self.canvas.create_oval(298,127, 312,140, width=2)
            self.delete_wave()
            self.draw_wave(6)
            
        elif len(self.game.body_parts_used) == 6:          #lower arm
            self.canvas.create_line(162,160, 170,205, 190,240, 192,241, width=2)
            self.canvas.create_line(181,175, 185,202, 202,234, 201,236, width=2)
            self.canvas.create_oval(192,235, 205,248, width=2)
            self.delete_wave()
            self.draw_wave(2)
            
        elif len(self.game.body_parts_used) == 3:          #upper leg
            self.canvas.create_line(275,168, 330,178, 362,235, 380,240, width=2)
            self.canvas.create_line(265,195, 315,200, 350,250, 378,252, 380,240, width=2)
            self.delete_wave()
            self.draw_wave(8)
            
        elif len(self.game.body_parts_used) == 4:        #lower leg
            self.canvas.create_line(270,196, 300,204, 328,256, 346,261, width=2)
            self.canvas.create_line(230,215, 279,221, 315,270, 344,273, 346,261, width=2)
            self.delete_wave()
            self.draw_wave(5)
            
        elif len((self.game.body_parts_used)) == 8:        #blood
            self.canvas.delete("all")
            self.canvas.create_line(80, 80, 80, 300, 420, 300, 420, 80, width=3)
            self.canvas.create_rectangle(82, 115, 419, 299, fill="red", width=0)
            """
            little test
            text1 = tk.Text(self.canvas)
            text1.pack()
            photo = tk.PhotoImage(file='test.jpg')
            text1.image_create(tk.END,image=photo)
            """
            self.delete_wave()
            self.draw_wave(10)
        
        
    def restart(self):
        """
        restart the game
        change text displayed on labels
        clear the canvas
        using  self.canvas.delete("all")
        """
        self.canvas.delete("all")
        self.label1.config(text = make_display_word(self.game.word,""))
        self.label2.config(bg = 'lightblue', text = "take a guess")
        self.game.start()
        self.start()
        
          
#This starts everything. 
Hangman_gui()










