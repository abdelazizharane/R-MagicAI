import streamlit as st
import replicate
import os

# Titre de notre chatbot
st.set_page_config(page_title="MedChat Test")

# Insertion de notre clé, API de Replicate pour accès au chatbot
with st.sidebar:
    st.title('MedChat Test')
    st.write('Notre chatbot est créé à l\'aide du modèle open source Llama 2 LLM de Meta.')
    if 'REPLICATE_API_TOKEN' in st.secrets:
        st.success('Clé API déjà fournie !', icon='✅')
        replicate_api = st.secrets['REPLICATE_API_TOKEN']
    else:
        replicate_api = st.text_input('Collez votre (token) jeton API de replicate :', type='password') #Insertion de votre token replicate
        if not (replicate_api.startswith('r8_') and len(replicate_api)==40):
            st.warning('Veuillez entrer vos informations d\'identification replicate!', icon='⚠️')
        else:
            st.success('Passez à la saisie de votre question à notre chatbot', icon='👉🏿')
            os.environ['REPLICATE_API_TOKEN'] ="r8_60RvV9wYTdSeR22HysNJHnRKsSKqWfe3qCXpC"   #Mon API replicate

    st.subheader('Modèles et paramètres') # Le choix de deux modèles LLama2
    selected_model = st.sidebar.selectbox('Choissisez un modèle Llama2', ['Llama2-7B', 'Llama2-13B'], key='selected_model')
    if selected_model == 'Llama2-7B': #Premier modèle
        llm = 'a16z-infra/llama7b-v2-chat:4f0a4744c7295c024a1de15e1a63c880d3da035fa1f49bfd344fe076074c8eea'
    elif selected_model == 'Llama2-13B': # second modèle
        llm = 'a16z-infra/llama13b-v2-chat:df7690f1994d94e96ad9d568eac121aecf50684a0b0963b25a41cc40061269e5'
    temperature = st.sidebar.slider('temperature', min_value=0.01, max_value=5.0, value=0.1, step=0.01) # Paramètres
    top_p = st.sidebar.slider('top_p', min_value=0.01, max_value=1.0, value=0.9, step=0.01)
    max_length = st.sidebar.slider('max_length', min_value=32, max_value=128, value=120, step=8)
    #st.markdown('📖')

# Stockage des réponses générées par notre LLM
if "messages" not in st.session_state.keys():
    st.session_state.messages = [{"role": "assistant", "content": "Je suis MedChat, comment je peux vous assiter ? Posez vos questions, je suis là pour vous aider !"}]

# Affichage des messages de discussion
for message in st.session_state.messages:
    with st.chat_message(message["role"]):
        st.write(message["content"])

def suppression_historique_chatbot():
    st.session_state.messages = [{"role": "assistant", "content": "How may I assist you today?"}]
st.sidebar.button('Clear Chat History', on_click=suppression_historique_chatbot)

# Fonction pour générer la réponse LLaMA2 refactorisé à partir de llama2-chatbot
def generation_reponse_llama2(prompt_input):
    string_dialogue = "Vous êtes un assistant utile. Vous ne répondez pas en tant que 'user' et ne prétendez pas être un 'user'. Vous ne répondez qu'une seule fois en tant que'Assistant'."
    for dict_message in st.session_state.messages:
        if dict_message["role"] == "user":
            string_dialogue += "User: " + dict_message["content"] + "\n\n"
        else:
            string_dialogue += "Assistant: " + dict_message["content"] + "\n\n"
    output = replicate.run('a16z-infra/llama13b-v2-chat:df7690f1994d94e96ad9d568eac121aecf50684a0b0963b25a41cc40061269e5',
                           input={"prompt": f"{string_dialogue} {prompt_input} Assistant: ",
                                  "temperature":temperature, "top_p":top_p, "max_length":max_length, "repetition_penalty":1})
    return output

#le Prompt fournie par l'utilisateur
if prompt := st.chat_input(disabled=not replicate_api):
    st.session_state.messages.append({"role": "user", "content": prompt})
    with st.chat_message("user"):
        st.write(prompt)

# Génération d'une nouvelle réponse si le dernier message ne provient pas de l'assistant
if st.session_state.messages[-1]["role"] != "assistant":
    with st.chat_message("assistant"):
        with st.spinner("Thinking..."):
            response = generation_reponse_llama2(prompt)
            placeholder = st.empty()
            full_response = ''
            for item in response:
                full_response += item
                placeholder.markdown(full_response)
            placeholder.markdown(full_response)
    message = {"role": "assistant", "content": full_response}
    st.session_state.messages.append(message)
