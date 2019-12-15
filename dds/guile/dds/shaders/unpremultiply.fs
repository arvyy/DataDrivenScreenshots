#version 330       
in vec2 fragTexCoord;              
in vec4 fragColor;                 
out vec4 finalColor;               
uniform sampler2D texture0;        
void main()                        
{                                  
    vec4 texelColor = texture(texture0, fragTexCoord);   
    vec4 color = texelColor;
    finalColor = vec4(color.xyz / color.a, color.a);
}
