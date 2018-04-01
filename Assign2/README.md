# Assignment 2 - Elm CV / App

## Elm CV

For my CV I decided to use a few different external resources. I used the 2 external stylesheets and one custom font was imported. Most of my work was done with the w3 schools stylesheet while following along with some of their example templates and editing it to fit a style I prefered. Links to all of these resources can be found here:

[Tutorial for using an external stylesheet](http://elmprogramming.com/building-a-simple-page-in-elm.html)

[W3-Schools Stylesheet](https://www.w3schools.com/w3css/4/w3.css)

[Custom Font](https://fonts.googleapis.com/css?family=Roboto)

[Other external stylesheet](https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css)

I decided to go with a w3-card based theme. I liked that it was very minimalist and kept information nicely separate. It allows the user to read through and clearly understand what each section is about and where it ends. I also used a variety of fa fa icons from the w3-schools css just to pretty up the page.

## App - Elm Tron

For my elm app, I recreated the classic [Tron](https://en.wikipedia.org/wiki/Tron_(video_game)) arcade game for two players. For those unfamiliar with this game, it is comprised of a field on which two bikes can ride. The bikes leave behind a trail everywhere they go and the object of the game is to avoid crashing into any trails (both yours and the opponents), the walls, or the other bike. The first one to crash, loses the round.

I took some inspiration from an [elm based Snake game](https://github.com/theburningmonk/elm-snake). I liked how they constructed their snake using custom position type and constructing the snake out of a head (singular position value) and a tail (list of position vlaues). However, they used the Collage package to render their game and I instead used SVG figures. This was partially due to the fact that I was more comfortable with using them but mostly because I still plan to further impleent the feature of power-ups that utilize SVG shapes' ability to rotate and change in a variety of ways (ex. rounding squares). 

For the purposes of drawing the snake I also had to use the List.map function which I was unfamiliar with so I took some inspiration from this stackoverflow response. [Link to response](https://stackoverflow.com/questions/39058189/how-to-iterate-over-a-list-in-an-elm-view)
