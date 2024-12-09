# text files

library(tidyverse)


df_text <- fread("text_updated.csv") |>
  drop_na() |>
  mutate(title_upper = str_to_upper(title))

saveRDS(df_text, "df_text.rds")


#=======================================================================================

# old text data

# create a df for text

df_text <- tibble(
  title = c(vulnerability,threats, news_engagement, engagement_news_outlets, engagement_politicians),
  subtitle = c(
    "How skewed is the CIE towards a small set of accounts?",
    "How divided is the CIE into distinct communities?",
    "How insular are major political parties relative to one another?",
    "How prevalent is toxic speech in the CIE?",
    "How people feel about their own political party versus other parties?",
    "To what extent do people avoid political and public affairs news?",
    "How much do people trust information gatekeepers?",
    "To what extent do people avoid political discussion and opinion sharing?",
    "How concerned are people about misinformation?",
    "How concerned are people about foreign influence?",
    "How concerned are people about artificial intelligence?",
    "To what extent are overt foreign influencers impacting CIE?",
    "How common and popular are web links to known misinformation websites?",
    "How often do people post about misinformation and foreign influence?",
    "How many people seek out information about news?",
    "How actively people share and post news on social media?",
    "How many people use each medium weekly for news?",
    "How many people use each source for weekly news?",
    "How many people use social media weekly for news?",
    "What are the top 5 Canadian news outlets? What is their share of total engagement?",
    "How skewed is preference for national vs local news?",
    "Which social media platforms are most popular for news?",
    "Which social media platforms are most popular for Canadian politicians?",
    "Which federal party leaders are most engaging with Canadians?",
    "Which elected party members are most engaging with Canadians?",
    "How skewed is engagement with politicians vs news outlets?"
  ),
  text = c("We evaluate inequality using the Gini coefficient 
           (the extent influence within an information ecosystem deviates from a perfectly even distribution). 
           The scale ranges from 0 (each entity has equal influence) to 1 (a single entity has all the influence). 
           Fewer information sources (high value) severely restrict information flows, which can lead to faster spread 
           and deeper penetration of misinformation.", 
           "We evaluate polarization through segmentation (the extent a network is divided into distinct communities, 
           0=low segmentation and 1=high segmentation).",
           "We evaluate insularity (the extent political party families engage with others outside their political party, with a higher number 
            representing a more insular party family).",
           "We evaluate toxicity through toxic speech (presence of toxic speech among posts by political influencers, with lower values indicating lower toxicity).",
           "Division: Text TBD",
           "We evaluate news avoidance (extent people avoid the news all together, 0=low to 1=high).",
           "We evaluate trust by asking Canadians about their confidence in information gatekeepers (journalists, media organizations, politicians, and big tech).",
           "We evaluate chilled speech (extent people avoid online political discussion and opinion sharing, 0=low to 1=high).",
           "We evaluate the threat of misinformation by measuring Canadians’ concern about misinformation.",
           "We evaluate the threat of foreign influence by measuring concern about foreign governments influencing Canadian media and politics (in general and by country).",
           "We assess the threat of generative AI by measuring concern about AI generated content misleading the general public.",
           "We assess how ‘close’ foreign accounts are to Canadian accounts in regards to the content they share: we can understand these accounts to be in close proximity to the CIE if they frequently post similar links or discuss certain topics at the same time and in the same way.",
           "We measure how much misinformation is circulated in the CIE by politically influential accounts. We do not measure how much misinformation in total there is in the CIE – that would require us to analyze every single post across multiple platforms in Canada.",
           "We analyze discussion about misinformation to assess the level of attention it is getting. Examining how often prominent accounts within the CIE provides an additional dimension to our analysis of concern about misinformation.",
           "We report the proportion of respondents who say “Occasionally seeking out”, “Often seeking out”, and “Constantly seeking out”.",
           "We calculate the proportion who say “Once or twice a week” or more and then average the proportions to generate an overall news sharing measure.",
           "We calculate the proportion who say “once or twice a week” or more to generate a news consumption score for each news medium.",
           "Text: TBD",
           "Text: TBD",
           "We first identify the five outlets that received the highest amount of engagement (as measured by the total number of likes, comments, and shares on their posts), sum them, then divide their engagement by the total amount of engagement received by Canadian news outlets online and multiplying by 100. We are then able to see how much of online Canadian news engagement is generated by the biggest outlets.",
           "We calculate the relative local news engagement by dividing the total amount of engagement generated by local news accounts across our platforms by the total amount of engagement generated by all news accounts.",
           "We evaluate on which platforms Canadian news outlets receive the most engagement to better understand where Canadians seek their online news content. We calculate the platform with the most engagement with Canadian news outlets by summing the total number of likes on posts by Canadian news outlets per platform, and reporting the platform with the most likes.",
           "We identify the most popular platform for engagement with politicians by adding up the total number of likes on posts by Canadian politicians across our platforms for the whole month, then identifying which platform generated the most likes.",
           "We compare the engagement rates of the leaders of Canadian federal political parties which currently hold seats in the House of Commons. This is calculated by summing the total amount of likes each leader receives on their posts across our platforms of interest, then representing these likes as a percentage of all likes received by all party leaders.",
           "We compare the engagement rates of all Canadian party ‘families’ (Liberal, NDP, Conservative, Bloc Quebecois, Green). We sum the total amount of engagement received by each party’s politicians, on both the federal and provincial level, then dividing that sum by the total amount of engagement received by Canadian politicians who are not party leaders.",
           "We report the percentage of engagement generated by Canadian politicians relative to Canadian news outlets to gain an idea of how prominent politicians are in the CIE. We calculate this percentage by dividing the total number of likes received by Canadian politicians by the total number of likes received by both news outlets and politicians.")
) |>
  mutate(title_upper = str_to_upper(title))

write_csv(df_text, "df_text.csv")


