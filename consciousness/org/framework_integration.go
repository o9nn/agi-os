package org
import (
	"context"
	"log"
	"sync"
	"time"
	"github.com/EchoCog/echollama/core/deeptreeecho"
)
var (
	GlobalIdentityFramework *OrganizationalIdentityFramework
	frameworkOnce           sync.Once
)
func InitializeGlobalFramework() *OrganizationalIdentityFramework {
	frameworkOnce.Do(func() {
		log.Println("🌳 Initializing Global Deep Tree Echo Identity Framework...")
		GlobalIdentityFramework = NewOrganizationalIdentityFramework()
		ctx := context.Background()
		if err := GlobalIdentityFramework.Initialize(ctx); err != nil {
			log.Printf("Error initializing framework: %v", err)
		}
		log.Println("✨ Global Identity Framework initialized successfully")
	})
	return GlobalIdentityFramework
}
func GetGlobalFramework() *OrganizationalIdentityFramework {
	if GlobalIdentityFramework == nil {
		return InitializeGlobalFramework()
	}
	return GlobalIdentityFramework
}
func IntegrateWithExistingIdentity(existingIdentity *deeptreeecho.Identity) {
	framework := GetGlobalFramework()
	if framework.CoreIdentity != nil && existingIdentity != nil {
		syncIdentityStates(framework.CoreIdentity, existingIdentity)
		updateFrameworkFromIdentity(framework, existingIdentity)
		log.Println("🔗 Identity framework integrated with existing identity system")
	}
}
func syncIdentityStates(frameworkIdentity, existingIdentity *deeptreeecho.Identity) {
	if existingIdentity.SpatialContext != nil {
		frameworkIdentity.SpatialContext = existingIdentity.SpatialContext
	}
	if existingIdentity.EmotionalState != nil {
		frameworkIdentity.EmotionalState = existingIdentity.EmotionalState
	}
	if existingIdentity.Memory != nil {
		for nodeID, node := range existingIdentity.Memory.Nodes {
			frameworkIdentity.Memory.Nodes[nodeID] = node
		}
		for edgeID, edge := range existingIdentity.Memory.Edges {
			frameworkIdentity.Memory.Edges[edgeID] = edge
		}
	}
	for patternName, pattern := range existingIdentity.Patterns {
		frameworkIdentity.Patterns[patternName] = pattern
	}
}
func updateFrameworkFromIdentity(framework *OrganizationalIdentityFramework, identity *deeptreeecho.Identity) {
	if framework.PersonaModel != nil && identity.EmotionalState != nil {
		framework.PersonaModel.EmotionalProfile.PrimaryEmotions[identity.EmotionalState.Primary.Type] =
			identity.EmotionalState.Primary.Strength
		framework.PersonaModel.EmotionalProfile.EmotionalIntensity = identity.EmotionalState.Intensity
	}
	if framework.AdaptationMetrics != nil {
		framework.AdaptationMetrics.FlexibilityScore = identity.Coherence
		framework.AdaptationMetrics.ConsistencyMaintenance = identity.Coherence
	}
	framework.LastUpdated = time.Now()
}
func ProcessThroughFramework(input string) (string, error) {
	framework := GetGlobalFramework()
	return framework.ProcessWithIdentity(input)
}
func GetFrameworkStatus() map[string]interface{} {
	framework := GetGlobalFramework()
	return framework.GetFrameworkStatus()
}
func SaveFrameworkState() error {
	framework := GetGlobalFramework()
	return framework.SaveFramework()
}
func LoadFrameworkState() error {
	framework := GetGlobalFramework()
	return framework.LoadFramework()
}