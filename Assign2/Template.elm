module BeginningElm exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

--Style for sidebar text
sideBarFont = style [("width", "100%" ),("font-size", "1vw")]

--General class general cards. Used in the classList attribute.
cardClass = "w3-white w3-card-4"

--The main font size I use in my cards.
mainCardFont = style [("font-size","16px")]

--Class information for footer items.
footerBarItem = "w3-large w3-text-indigo w3-hover-opacity w3-margin"

--View
view model =
  -- The overall grey background card
  div [class "w3-card w3-grey w3-margin"]
  [
     -- The actual main area in which all my content is held.
     div [ class "w3-content w3-margin-top", style [("max-width","1400px")]]
     [
        div [ class "w3-row-padding w3-margin-top" ]
        [
          -- The div for the items in the left sidebar that take up the first third of the screen.
          -- I use vw font sizes for proper resizing on the webpage.
          div [ class "w3-third w3-margin-top", style [("font-size","1.5vw")] ]
          [
            -- The profile card at the top left.
            div [ classList [(cardClass,True),("w3-text-grey",False)] ]
                [ div [ class "w3-display-container"]
                    [ img [ alt "Profile Picture", src "profile.png", style [("width","100%")] ]
                        []
                    ],
                  div [class "w3-container"]
                  [
                    h3 [ class "w3-text-indigo w3-padding-16 w3-margin-left" ] [text "Daniel Rubinstein"]
                  ],
                  div [ class "w3-container w3-margin-left" ]
                  [
                    p [mainCardFont]
                        [ i [ class "fa fa-briefcase fa-fw w3-margin-right w3-large w3-text-indigo" ]
                            []
                        , text "McMaster Student - Computer Science"
                        ]
                    , p [mainCardFont]
                        [ i [ class "fa fa-home fa-fw w3-margin-right w3-large w3-text-indigo" ]
                            []
                        , text "Richmond Hill, ON, CA"
                        ]
                    , p [mainCardFont]
                        [ i [ class "fa fa-envelope fa-fw w3-margin-right w3-large w3-text-indigo" ]
                            []
                        , a [href "mailto:rubins.daniel@gmail.com"]
                            [text "rubins.daniel@gmail.com"]
                        ]
                    , p [mainCardFont]
                        [ i [ class "fa fa-phone fa-fw w3-margin-right w3-large w3-text-indigo" ]
                            []
                        , a [href "tel:+16479959319"]
                            [text "647-995-9319"]
                        ]
                    , p [mainCardFont]
                       [ i [ class "fa fa-linkedin-square fa-fw w3-margin-right w3-large w3-text-indigo"]
                            []
                        ,a [href "https://www.linkedin.com/in/daniel-rubinstein"]
                          [text "www.linkedin.com/in/daniel-rubinstein"]
                       ]
                    , p [mainCardFont]
                       [ i [ class "fa fa-github-square fa-fw w3-margin-right w3-large w3-text-indigo"]
                            []
                        ,a [href "https://github.com/Rubinstd"]
                           [text "https://github.com/Rubinstd"]
                       ]
                    ]
                ],
                --The skills card at the middle left of the screen.
                div [ classList [(cardClass,True),("w3-margin-top",True),("w3-margin-bottom",True),("w3-container",True)]]
                [
                  h3 [ class "w3-text-grey w3-padding-16 w3-margin-left" ]
                   [ i [ class "fa fa-keyboard-o fa-fw w3-margin-right w3-xxlarge w3-text-indigo" ]
                       []
                   , text "Skills"
                   ],
                   div [class "w3-container"]
                   [
                      h6 [ class " w3-large w3-text-indigo"]
                      [
                        text "Proficient in: "
                      ],
                      ul [mainCardFont]
                      [
                        li []
                        [text "Java"],
                        li []
                        [text "Haskell"],
                        li []
                        [text "Objective-C"],
                        li []
                        [text "C#"],
                        li []
                        [text "T-SQL"],
                        li []
                        [text "Elm"],
                        li []
                        [text "Bash scripting"],
                        li []
                        [text "Microsoft Office"],
                        li []
                        [text "Photo/Video Editing"]
                      ],
                      h6 [ class " w3-large w3-text-indigo"]
                      [
                          text "Possesses basic knowledge of: "
                      ],
                      ul [mainCardFont]
                      [
                        li []
                        [text "JavaScript"],
                        li []
                        [text "Visual Basic"],
                        li []
                        [text "Swift"],
                        li []
                        [text "R"],
                        li []
                        [text "RobotC"],
                        li []
                        [text "Maple"],
                        li []
                        [text "Photo/Video Editing"]
                      ]
                   ]
                ],
                --The awards card at on the left sidebar.
                div [ classList [(cardClass,True),("w3-margin-top",True),("w3-margin-bottom",True),("w3-container",True)]]
                [
                  h3 [ class "w3-text-grey w3-padding-16 w3-margin-left" ]
                   [ i [ class "fa fa-certificate fa-fw w3-margin-right w3-xxlarge w3-text-indigo" ]
                       []
                   , text "Awards"
                   ],
                   div [class "w3-container"]
                   [
                      h6 [ class " w3-large w3-text-indigo"]
                      [
                        text "The Academy For Gifted Children P.A.C.E."
                      ],
                      ul [mainCardFont]
                      [
                        li []
                        [text "Platinum Honor Roll - Awarded to those with an average of 90%+"],
                        li []
                        [text "Theatre Technicians Award - Awarded to the individual who runs the theatre’s technician group."],
                        li []
                        [text "CCC Top Participant Award - Awarded to those who scored at the top of the school in the CCC."]
                      ],
                      h6 [ class " w3-large w3-text-indigo"]
                      [
                          text "McMaster University"
                      ],
                      ul [mainCardFont]
                      [
                        li []
                        [text "Undergraduate Student Research Award - Selected from top 100 applicants ($5000)"],
                        li []
                        [text "McMaster Engineering Dean’s Excellence Entrance Scholarship ($7500)"],
                        li []
                        [text "McMaster President’s Award - Awarded for an application average of 95%+ ($2500)"]
                      ]
                   ]
                ],
                --The Project Links card on the left sidebar.
                div [ classList [(cardClass,True),("w3-margin-bottom",True),("w3-container",True)]]
                [
                  h3 [ class "w3-text-grey w3-padding-16 w3-margin-left" ]
                   [ i [ class "fa fa-desktop fa-fw w3-margin-right w3-xxlarge w3-text-indigo" ]
                       []
                   , text "Project Links"
                   ],
                   div [class "w3-container"]
                   [
                      h6 [ class " w3-large w3-text-indigo"]
                      [
                        a [href "ElmTron.html"]
                          [text "ElmTron"]
                      ],
                      ul [mainCardFont]
                      [
                        li []
                        [text "An elm app port of the classic Tron game!"],
                        li []
                        [a [href "https://github.com/Rubinstd/CS1XA3/tree/master/Assign2"] [text "Source Code"]]
                      ],
                      h6 [ class " w3-large w3-text-indigo"]
                      [
                          a [href "https://github.com/Rubinstd/CS1XA3/tree/master/Assign1"]
                            [text "Project Analyzer"]
                      ],
                      ul [mainCardFont]
                      [
                        li []
                        [text "A bash script to analyze/organize git repositories."]
                      ]
                   ]
                ]
          ],
          --The div for the cards more towards the right of the screen. They take up two thirds of the screen.
          --Uses vw font sizes for proper resizing.
          div [class "w3-twothird w3-margin-top", style [("font-size","1.5vw")]]
          [
            --The Highlights of Qualifications card.
            div [ classList [(cardClass,True),("w3-margin-bottom",True),("w3-container",True)] ]
            [
               h3 [ class "w3-text-grey w3-padding-16 w3-margin-left" ]
                [ i [ class "fa fa-question-circle fa-fw w3-margin-right w3-xxlarge w3-text-indigo" ]
                    []
                , text "Highlights of Qualifications"
                ],
                div [class "w3-container",mainCardFont]
                [
                  ul[]
                  [
                    li []
                    [text "Excellent communication and teamwork skills developed as a volunteer at a Senior Center."],
                    li []
                    [text "Proficient in various computer / information technology (IT) skills and Database Design as seen in project work including the management of SQL Server Databases, development in the .NET framework, all while exhibiting knowledge of Windows software design standards (as defined by documentation published by Microsoft)."]
                  ]
                ]
            ],
            --The Education card.
            div [ classList [(cardClass,True),("w3-margin-bottom",True),("w3-container",True)]]
            [
              h3 [ class "w3-text-grey w3-padding-16 w3-margin-left" ]
               [ i [ class "fa fa-book fa-fw w3-margin-right w3-xxlarge w3-text-indigo" ]
                   []
               , text "Education"
               ],
               div [class "w3-container"]
               [
                  h6 [ class "w3-large w3-text-indigo w3-text-indigo"]
                  [
                    text "Bachelor of Applied Sciences, Computer Science (Co-op):  Expected Graduation April 2021"
                  ],
                  h6 [class "w3-medium w3-text-indigo w3-text-black"]
                  [
                    text "McMaster University, Hamilton, ON"
                  ],
                  ul [mainCardFont]
                  [
                    li []
                    [text "Currently in level 1 of a 4-year program."],
                    li[]
                    [text "Completed 5 haskell programming assignments."],
                    li[]
                    [text "Completed 5 Maple Software assignments."],
                    li []
                    [text "Completed the WHMIS Lab Safety Course"]
                  ],
                  h6 [ class "w3-large w3-text-indigo"]
                  [
                    text "High School Diploma: Graduated  June 2017"
                  ],
                  h6 [class "w3-medium w3-text-indigo w3-text-black"]
                  [
                    text "The Academy for Gifted Children P.A.C.E., Richmond Hill, ON"
                  ],
                  ul [mainCardFont]
                  [
                    li []
                    [text "Competed in numerous mathematics competitions."],
                    li []
                    [text "Developed a finalist project for the SAS Data Analytics Competition"],
                    li []
                    [text "Participate in the annual Canadian Computing Competition scoring  in the top 3 of the school."],
                    li []
                    [text "Participated in 4 years of VEX robotics winning multiple design awards."]
                  ]


               ]
            ],
            --The work experience card.
            div [ classList [(cardClass,True),("w3-margin-bottom",True),("w3-container",True)]]
            [
              h3 [ class "w3-text-grey w3-padding-16 w3-margin-left" ]
               [ i [ class "fa fa-cogs fa-fw w3-margin-right w3-xxlarge w3-text-indigo" ]
                   []
               , text "Work Experience"
               ],
               div [class "w3-container w3-display-container"]
               [
                  h6 [ class "w3-large w3-text-indigo"]
                  [
                    text "Kumon Math Tutor - Richmond Hill ON",
                    div [class "w3-right w3-margin-right"]
                    [
                      text "June 2015 - July 2016"
                    ]
                  ],
                  ul [mainCardFont]
                  [
                    li []
                    [text "Developed strong verbal communication and teamwork skills while teaching alongside peers and managers."]
                  ],
                  h6 [ class " w3-large w3-text-indigo"]
                  [
                    text "Private Tennis Coach - Richmond Hill ON",
                    div [class "w3-right w3-margin-right"]
                    [
                      text "June 2012 - August 2016"
                    ]
                  ],
                  ul [mainCardFont]
                  [
                    li []
                    [text "Developed exceptional problem solving and adaptability skills while creating lesson plans for children age 5+ with a variety of learning styles."]
                  ]
               ]
            ],
            -- The Volunteer Experience card.
            div [ classList [(cardClass,True),("w3-margin-bottom",True),("w3-container",True)]]
            [
              h3 [ class "w3-text-grey w3-padding-16 w3-margin-left" ]
               [ i [ class "fa fa-cog fa-fw w3-margin-right w3-xxlarge w3-text-indigo" ]
                   []
               , text "Volunteer Experience"
               ],
               div [class "w3-container w3-display-container"]
               [
                  h6 [ class "w3-large w3-text-indigo"]
                  [
                    text "Senior Center Volunteer - Richmond Hill, ON",
                    div [class "w3-right w3-margin-right"]
                    [
                      text "June 2015 - August 2016"
                    ]
                  ],
                  ul [mainCardFont]
                  [
                    li []
                    [text "Utilized excellent leadership skills when directing a group that worked closely with individuals with Alzheimer’s."],
                    li []
                    [text "Implemented a variety of programs that encouraged the seniors to participate in holiday themed events."]
                  ],
                  h6 [ class " w3-large w3-text-indigo"]
                  [
                    text "Prostate Cancer Canada Volunteer - Richmond Hill, ON",
                    div [class "w3-right w3-margin-right"]
                    [
                      text "June 2014"
                    ]
                  ],
                  ul [mainCardFont]
                  [
                    li []
                    [text "Assisted in the coordination of a charity marathon for prostate cancer awareness displaying strong organizational skills."],
                    li []
                    [text "Motivated runners throughout the event while working in a team."]
                  ]


               ]
            ],
            --The Extracurricular card.
            div [ classList [(cardClass,True),("w3-margin-bottom",True),("w3-container",True)]]
            [
              h3 [ class "w3-text-grey w3-padding-16 w3-margin-left" ]
               [ i [ class "fa fa-paint-brush fa-fw w3-margin-right w3-xxlarge w3-text-indigo" ]
                   []
               , text "Extracurricular Activities"
               ],
               div [class "w3-container w3-display-container"]
               [
                  h6 [ class "w3-large w3-text-indigo"]
                  [
                    text "Guitarist and Pianist - Richmond Hill, ON",
                    div [class "w3-right w3-margin-right"]
                    [
                      text "January 2013 - June 2017"
                    ]
                  ],
                  ul [mainCardFont]
                  [
                    li []
                    [text "Participated and coordinated multiple performing bands as well as composed and played music in a variety of styles."],
                    li []
                    [text "Improvised jazz solos during performances displaying on the spot thinking abilities."]
                  ],
                  h6 [ class " w3-large w3-text-indigo"]
                  [
                    text "VEX Robotics Team Mentor/Leader - Richmond Hill, ON",
                    div [class "w3-right w3-margin-right"]
                    [
                      text "January 2013 - June 2016"
                    ]
                  ],
                  ul [mainCardFont]
                  [
                    li []
                    [text "Taught 2 teams of students (grades 4-12) in basic principles of robotics and assisted them at tournament events in which they placed at the top of their division as well as won numerous design awards."]
                  ]


               ]
            ],
            --The Personal Projects card.
            div [ classList [(cardClass,True),("w3-margin-bottom",True),("w3-container",True)]]
            [
              h3 [ class "w3-text-grey w3-padding-16 w3-margin-left" ]
               [ i [ class "fa fa-laptop fa-fw w3-margin-right w3-xxlarge w3-text-indigo" ]
                   []
               , text "Personal Projects"
               ],
               div [class "w3-container"]
               [
                  h6 [ class " w3-large w3-text-indigo"]
                  [
                    text "Custom Function Calculator"
                  ],
                  ul [mainCardFont]
                  [
                    li []
                    [text "Programmed customized calculator software in the .NET framework that was optimized for quick calculation and user created functions. Complied with all Microsoft software development standards. Coded primarily in C#."]
                  ],
                  h6 [ class " w3-large w3-text-indigo"]
                  [
                    text "Task Manager Software"
                  ],
                  ul [mainCardFont]
                  [
                    li []
                    [text "Developed software in the .NET framework (primarily C#)  that communicated to SQL Servers generating queries in T-SQL to save/retrieve information (stored as RichText) allowing users to manage their task lists."]
                  ],
                  h6 [ class " w3-large w3-text-indigo"]
                  [
                    text "Gantt Chart Software"
                  ],
                  ul [mainCardFont]
                  [
                    li []
                    [text "Created Gantt Chart graphing software that easily allowed users to import their own data source and control varying aspects of the outputted graphics. Coded in C# within the .NET framework adding the ability to import information from SQL server tables."]
                  ],
                  h6 [ class " w3-large w3-text-indigo"]
                  [
                    text "Text Parsing Software"
                  ],
                  ul [mainCardFont]
                  [
                    li []
                    [text "Designed software that could take in a block of text from a given data source and parse through it in order to retrieve desired information as was indicated by user defined specifications."]
                  ]


               ]
            ]


          ]
        ],
        --The footer. Contains basic contact info on buttons that change opacity when hovered over as well as basic info about when the webpage was created / the author.
        footer [ class "w3-container w3-padding-64 w3-center w3-opacity w3-light-grey w3-xlarge"]
        [
          a [href "https://www.linkedin.com/in/daniel-rubinstein", classList [(footerBarItem,True),("fa fa-linkedin-square",True)]][],
          a [href "https://github.com/Rubinstd", classList [(footerBarItem,True),("fa fa-github-square",True)]][],
          a [href "mailto:rubins.daniel@gmail.com", classList [(footerBarItem,True),("fa fa-envelope",True)]][],
          a [href "tel:+16479959319", classList [(footerBarItem,True),("fa fa-phone",True)]][],
          p [mainCardFont] [text "Author: Daniel Rubinstein"],
          p [mainCardFont] [text "Created: March 2018"]
        ]
      ]
  ]




main =
    view "CV model"
