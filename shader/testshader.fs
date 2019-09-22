#version 330       
in vec2 fragTexCoord;              
in vec4 fragColor;                 
out vec4 finalColor;               
uniform sampler2D texture0;        
uniform vec4 colDiffuse;           
uniform sampler2D mask;
void main()                        
{                                  
    vec4 texelColor = texture(texture0 , fragTexCoord);   
    vec4 alphaColor = texture(mask, fragTexCoord);
    finalColor = texelColor * alphaColor;
}
